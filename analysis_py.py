import os
from pathlib import Path
import pandas as pd
import numpy as np

# avoid font cache errors in sandboxed env (set before importing matplotlib)
os.environ.setdefault("MPLCONFIGDIR", str(Path("out_py") / "mpl_cache"))
(Path("out_py") / "mpl_cache").mkdir(parents=True, exist_ok=True)

import matplotlib.pyplot as plt
import statsmodels.api as sm

# Paths
DATA_PATHS = {
    "CH": "data/bis_ch_quarterly.csv",
    "HK": "data/bis_hk_quarterly.csv",
    "SG": "data/bis_sg_quarterly.csv",
    "GB": "data/bis_uk_quarterly.csv",
    "NL": "data/bis_nl_quarterly.csv",
    "IE": "data/bis_ie_quarterly.csv",
    "LU": "data/bis_lu_quarterly.csv",
    "SA": "data/Saudi_Arabia_cross_lia.csv",
}

OUT_DIR = Path("out_py")
FIG_DIR = Path("fig_py")
OUT_DIR.mkdir(exist_ok=True)
FIG_DIR.mkdir(exist_ok=True)

Q_START = "2015Q1"
Q_END = "2025Q2"
Q_TREAT = "2022Q2"


def parse_quarter(x):
    if pd.isna(x):
        return pd.Period("1970Q1", freq="Q")
    s = str(x).strip()
    if len(s) == 10 and s[4] == "-" and s[7] == "-":
        try:
            return pd.Period(pd.to_datetime(s), freq="Q")
        except Exception:
            return pd.NaT
    if "Q" in s:
        s = s.replace(" ", "")
        if "Q" not in s.upper():
            return pd.NaT
        s = s.upper().replace("Q", "Q")
    return pd.Period(s.replace(" ", ""), freq="Q")


def read_series(path, cid):
    if not Path(path).exists():
        return None
    df = pd.read_csv(path)
    cols = {c.lower(): c for c in df.columns}
    if "time_period" in cols and "obs_value" in cols:
        df = df.rename(columns={cols["time_period"]: "quarter", cols["obs_value"]: "value"})
    elif "quarter" in cols and "value" in cols:
        df = df.rename(columns={cols["quarter"]: "quarter", cols["value"]: "value"})
    else:
        return None
    df["quarter"] = df["quarter"].apply(parse_quarter)
    df = df.loc[df["quarter"].notna()]
    df["id"] = cid
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    return df[["id", "quarter", "value"]]


def load_panel():
    frames = []
    for cid, p in DATA_PATHS.items():
        df = read_series(p, cid)
        if df is not None:
            frames.append(df)
    panel = pd.concat(frames, ignore_index=True)
    panel = panel.dropna(subset=["value"])
    panel = panel.loc[(panel["quarter"] >= pd.Period(Q_START, freq="Q")) & (panel["quarter"] <= pd.Period(Q_END, freq="Q"))]
    panel = panel.groupby(["id", "quarter"], as_index=False)["value"].mean()
    panel["log_value"] = np.log(panel["value"])
    return panel


def auto_donors(panel):
    pre = panel.loc[panel["quarter"] < pd.Period(Q_TREAT, freq="Q")]
    stats = pre.groupby("id", group_keys=False).apply(
        lambda g: pd.Series({
            "mean_log": g["log_value"].mean(),
            "slope": np.polyfit(g["quarter"].astype(int), g["log_value"], 1)[0] if len(g) >= 2 else np.nan
        })
    ).reset_index()
    ch_row = stats.loc[stats["id"] == "CH"]
    if ch_row.empty:
        return ["GB", "NL", "IE", "LU"]
    stats = stats.loc[~stats["id"].isin(["CH", "HK", "SG", "SA"])]  # exclude treated/alt centers
    stats["dist"] = np.sqrt((stats["mean_log"] - ch_row["mean_log"].iloc[0]) ** 2 + (stats["slope"] - ch_row["slope"].iloc[0]) ** 2)
    donors = stats.sort_values("dist").dropna(subset=["dist"]).head(5)["id"].tolist()
    return donors or ["GB", "NL", "IE", "LU"]


def twfe_cluster(df, y, x, fe_ids):
    dummies = []
    for fe in fe_ids:
        col = df[fe]
        if pd.api.types.is_period_dtype(col):
            col = col.astype(str)
        d = pd.get_dummies(col, prefix=fe, drop_first=True)
        dummies.append(d)
    X = pd.concat([df[x].astype(float)] + dummies, axis=1)
    X = X.apply(pd.to_numeric, errors="coerce").fillna(0.0)
    X = sm.add_constant(X, has_constant="add")
    y_vec = pd.to_numeric(df[y], errors="coerce")
    model = sm.OLS(y_vec.astype(float), X.astype(float))
    res = model.fit(cov_type="cluster", cov_kwds={"groups": df[fe_ids[0]].astype(str)})
    return res


def triad_share(panel):
    triad = panel.loc[panel["id"].isin(["CH", "HK", "SG"])].copy()
    triad["triad_total"] = triad.groupby("quarter")["value"].transform("sum")
    triad["share"] = triad["value"] / triad["triad_total"]
    triad["post"] = (triad["quarter"] >= pd.Period(Q_TREAT, freq="Q")).astype(int)
    triad["treat_CH"] = (triad["id"] == "CH").astype(int)
    triad["group"] = np.where(triad["id"] == "CH", "CH", "HKSG")
    grouped = triad.groupby(["group", "quarter", "post"], as_index=False)["share"].sum()
    grouped["group_bin"] = (grouped["group"] == "CH").astype(int)
    grouped["post_treat"] = grouped["post"] * grouped["group_bin"]
    xcol = "post_treat"
    res = twfe_cluster(grouped, "share", xcol, ["group", "quarter"])
    tidy = pd.DataFrame({
        "term": ["post:treat_CH"],
        "estimate": [res.params[xcol]],
        "std.error": [res.bse[xcol]],
        "p.value": [res.pvalues[xcol]]
    })
    tidy.to_csv(OUT_DIR / "did_triad_share_CH_HKSG_py.csv", index=False)
    ci_low = tidy["estimate"] - 1.96 * tidy["std.error"]
    ci_high = tidy["estimate"] + 1.96 * tidy["std.error"]
    plt.figure(figsize=(4, 3))
    plt.errorbar(["CH vs HK+SG"], tidy["estimate"], yerr=[tidy["estimate"] - ci_low, ci_high - tidy["estimate"]],
                 fmt="o", color="black", ecolor="black")
    plt.axhline(0, ls="--", color="gray")
    plt.ylabel("Estimate")
    plt.title("DiD triad share (CH vs HK+SG)")
    plt.tight_layout()
    plt.savefig(FIG_DIR / "did_triad_share_CH_HKSG_py.png", dpi=200)
    plt.close()


def global_share(panel, donors):
    triad_donors = panel.loc[panel["id"].isin(["CH", "HK", "SG"] + donors)].copy()
    triad_donors["group"] = np.where(triad_donors["id"] == "CH", "CH",
                                     np.where(triad_donors["id"].isin(["HK", "SG"]), "HKSG", "Donor"))
    triad_donors = triad_donors.groupby(["group", "quarter"], as_index=False)["value"].sum()
    triad_donors["global_total"] = triad_donors.groupby("quarter")["value"].transform("sum")
    triad_donors["global_share"] = triad_donors["value"] / triad_donors["global_total"]
    triad_donors["post"] = (triad_donors["quarter"] >= pd.Period(Q_TREAT, freq="Q")).astype(int)
    triad_donors["group"] = pd.Categorical(triad_donors["group"], categories=["Donor", "CH", "HKSG"])
    dummies = pd.get_dummies(triad_donors["group"], drop_first=True).astype(float)
    post_num = triad_donors["post"].astype(float)
    X = pd.DataFrame({col: post_num * dummies[col] for col in dummies.columns})
    X = sm.add_constant(X.fillna(0.0).astype(float), has_constant="add")
    y = triad_donors["global_share"].astype(float)
    model = sm.OLS(y, X)
    res = model.fit(cov_type="cluster", cov_kwds={"groups": triad_donors["group"].astype(str)})
    terms = [c for c in X.columns if c != "const"]
    tidy = pd.DataFrame({
        "term": terms,
        "estimate": res.params[terms],
        "std.error": res.bse[terms],
        "p.value": res.pvalues[terms]
    })
    tidy.to_csv(OUT_DIR / "did_global_share_CH_HKSG_vs_donors_py.csv", index=False)
    plt.figure(figsize=(4, 3))
    plt.errorbar(tidy["term"], tidy["estimate"], yerr=1.96 * tidy["std.error"], fmt="o", color="black")
    plt.axhline(0, ls="--", color="gray")
    plt.xticks(rotation=20)
    plt.ylabel("Estimate")
    plt.title("DiD global share vs donors")
    plt.tight_layout()
    plt.savefig(FIG_DIR / "did_global_share_CH_HKSG_vs_donors_py.png", dpi=200)
    plt.close()


def log_liab_did(panel, donors):
    df = panel.loc[panel["id"].isin(["CH"] + donors)].copy()
    df["treated"] = (df["id"] == "CH").astype(int)
    df["post"] = (df["quarter"] >= pd.Period(Q_TREAT, freq="Q")).astype(int)
    df["group"] = np.where(df["treated"] == 1, "CH", "Donor")
    grouped = df.groupby(["group", "quarter", "post"], as_index=False)["log_value"].mean()
    grouped["group"] = pd.Categorical(grouped["group"], categories=["Donor", "CH"])
    dummies = pd.get_dummies(grouped["group"], drop_first=True).astype(float)
    post_num = grouped["post"].astype(float)
    X = pd.DataFrame({col: post_num * dummies[col] for col in dummies.columns})
    X = sm.add_constant(X.fillna(0.0).astype(float), has_constant="add")
    y = grouped["log_value"].astype(float)
    mask = ~(y.isna() | X.isna().any(axis=1))
    model = sm.OLS(y[mask], X[mask])
    res = model.fit(cov_type="cluster", cov_kwds={"groups": grouped.loc[mask, "group"].astype(str)})
    term = [c for c in X.columns if c != "const"][0]
    tidy = pd.DataFrame({
        "term": [term],
        "estimate": [res.params[term]],
        "std.error": [res.bse[term]],
        "p.value": [res.pvalues[term]]
    })
    tidy.to_csv(OUT_DIR / "did_log_CH_vs_donors_py.csv", index=False)
    plt.figure(figsize=(4, 3))
    plt.errorbar(["CH vs donors"], tidy["estimate"], yerr=1.96 * tidy["std.error"], fmt="o", color="black")
    plt.axhline(0, ls="--", color="gray")
    plt.ylabel("Estimate (log)")
    plt.title("DiD log liabilities CH vs donors")
    plt.tight_layout()
    plt.savefig(FIG_DIR / "did_log_CH_vs_donors_py.png", dpi=200)
    plt.close()


def placebo_did(panel, donors):
    rows = []
    for u in donors:
        dat = panel.loc[panel["id"].isin([u] + [d for d in donors if d != u])].copy()
        dat["treated"] = (dat["id"] == u).astype(int)
        dat["post"] = (dat["quarter"] >= pd.Period(Q_TREAT, freq="Q")).astype(int)
        dat["group"] = np.where(dat["treated"] == 1, "treated", "control")
        grouped = dat.groupby(["group", "quarter", "post"], as_index=False)["log_value"].mean()
        grouped["group"] = pd.Categorical(grouped["group"], categories=["control", "treated"])
        dummies = pd.get_dummies(grouped["group"], drop_first=True).astype(float)
        post_num = grouped["post"].astype(float)
        X = pd.DataFrame({col: post_num * dummies[col] for col in dummies.columns})
        X = sm.add_constant(X.fillna(0.0).astype(float), has_constant="add")
        y = grouped["log_value"].astype(float)
        mask = ~(y.isna() | X.isna().any(axis=1))
        model = sm.OLS(y[mask], X[mask])
        res = model.fit(cov_type="cluster", cov_kwds={"groups": grouped.loc[mask, "group"].astype(str)})
        term = [c for c in X.columns if c != "const"][0]
        rows.append({"unit": u, "estimate": res.params[term], "std.error": res.bse[term]})
    placebo_df = pd.DataFrame(rows)
    placebo_df.to_csv(OUT_DIR / "did_placebo_log_results_py.csv", index=False)
    plt.figure(figsize=(6, 4))
    plt.errorbar(placebo_df["unit"], placebo_df["estimate"], yerr=1.96 * placebo_df["std.error"], fmt="^", color="black")
    plt.axhline(0, ls="--", color="gray")
    plt.ylabel("Estimate")
    plt.title("DiD placebo: donors")
    plt.tight_layout()
    plt.savefig(FIG_DIR / "fig_did_placebo_donors_py.png", dpi=200)
    plt.close()


def main():
    panel = load_panel()
    donors = auto_donors(panel)
    triad_share(panel)
    global_share(panel, donors)
    log_liab_did(panel, donors)
    placebo_did(panel, donors)
    panel.to_csv(OUT_DIR / "panel_py.csv", index=False)
    print("Python outputs written to out_py/ and fig_py/. Donors:", donors)


if __name__ == "__main__":
    main()
