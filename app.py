# ============================================================
# CEBU CITY DENGUE FORECASTING APP
# Designer version — no black header/table, improved sidebar, no accuracy score
# ============================================================

import html
import json
import glob
import shutil
import subprocess
import time
import tempfile
import zipfile
from pathlib import Path
from typing import Any, Dict, List, Optional

import folium
import geopandas as gpd
import pandas as pd
import streamlit as st
import streamlit.components.v1 as components
from streamlit_folium import st_folium


# ============================================================
# PAGE CONFIG
# ============================================================

st.set_page_config(
    page_title="Cebu City Dengue Forecasting App",
    page_icon="🦟",
    layout="wide",
    initial_sidebar_state="expanded",
)


# ============================================================
# PATHS
# ============================================================

BASE_DIR = Path(__file__).resolve().parent

DATA_DIR = BASE_DIR / "data"
MODEL_DIR = BASE_DIR / "models"
METADATA_DIR = BASE_DIR / "model_metadata"
R_SCRIPTS_DIR = BASE_DIR / "r_scripts"

DATASET_PATH = DATA_DIR / "FINAL_DATASET.xlsx"
SHAPE_ZIP_PATH = DATA_DIR / "cebu_city_barangays.zip"
R_SCRIPT_PATH = R_SCRIPTS_DIR / "predict_on_demand.R"
R_PACKAGE_SETUP_PATH = BASE_DIR / "r_packages_setup.R"

DISPLAY_HORIZONS = [0, 1, 2, 3, 4, 8, 12]


# ============================================================
# CSS
# ============================================================

st.markdown(
    """
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700;800;900&display=swap');

    :root {
        --ink: #111827;
        --muted: #4b5563;
        --soft-muted: #6b7280;
        --purple: #6d28d9;
        --blue: #2563eb;
        --green: #16a34a;
        --red: #dc2626;
        --amber: #f59e0b;
        --panel: rgba(255, 255, 255, 0.72);
        --panel-strong: rgba(255, 255, 255, 0.88);
        --line: rgba(17, 24, 39, 0.10);
    }

    html, body, [class*="css"] {
        font-family: 'Inter', sans-serif !important;
        color: var(--ink) !important;
    }

    .stApp {
        background:
            radial-gradient(circle at 10% 8%, rgba(255, 186, 73, 0.44), transparent 28%),
            radial-gradient(circle at 92% 8%, rgba(124, 58, 237, 0.28), transparent 33%),
            radial-gradient(circle at 70% 82%, rgba(20, 184, 166, 0.22), transparent 38%),
            linear-gradient(135deg, #fff7ed 0%, #f8fafc 44%, #eef2ff 100%);
    }

    /* Removes the heavy black top bar and replaces it with a soft aurora strip. */
    header[data-testid="stHeader"] {
        background:
            linear-gradient(90deg, rgba(255, 247, 237, 0.92), rgba(238, 242, 255, 0.92), rgba(236, 253, 245, 0.92)) !important;
        backdrop-filter: blur(18px) !important;
        border-bottom: 1px solid rgba(17, 24, 39, 0.08) !important;
    }

    header[data-testid="stHeader"] * {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    [data-testid="stToolbar"] * {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    /* App max width */
    .block-container {
        padding-top: 3.4rem !important;
        padding-bottom: 3rem !important;
        max-width: 1480px !important;
    }

    /* ========================= SIDEBAR ========================= */

    section[data-testid="stSidebar"] {
        background:
            radial-gradient(circle at 15% 5%, rgba(109, 40, 217, 0.12), transparent 26%),
            radial-gradient(circle at 85% 20%, rgba(22, 163, 74, 0.11), transparent 30%),
            linear-gradient(180deg, #fff7ed 0%, #f8fafc 46%, #eef2ff 100%) !important;
        border-right: 1px solid rgba(17, 24, 39, 0.10);
        box-shadow: 15px 0 45px rgba(17, 24, 39, 0.06);
    }

    section[data-testid="stSidebar"] > div {
        padding-top: 2rem !important;
    }

    section[data-testid="stSidebar"] label,
    section[data-testid="stSidebar"] p,
    section[data-testid="stSidebar"] span,
    section[data-testid="stSidebar"] div {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    .sidebar-title-card {
        background: rgba(255,255,255,0.76);
        border: 1px solid rgba(255,255,255,0.92);
        border-radius: 26px;
        padding: 1.1rem 1.15rem;
        box-shadow: 0 16px 38px rgba(17,24,39,0.08);
        margin-bottom: 1rem;
    }

    .sidebar-title {
        font-size: 1.18rem;
        letter-spacing: -0.03em;
        font-weight: 950;
        color: var(--ink);
        margin-bottom: .25rem;
    }

    .sidebar-subtitle {
        color: var(--muted);
        font-size: .86rem;
        line-height: 1.45;
        font-weight: 600;
    }

    .game-chip-row {
        display: flex;
        gap: .45rem;
        flex-wrap: wrap;
        margin-top: .8rem;
    }

    .game-chip {
        background: linear-gradient(135deg, rgba(237,233,254,.95), rgba(220,252,231,.95));
        border: 1px solid rgba(255,255,255,.9);
        color: var(--ink);
        border-radius: 999px;
        padding: .34rem .58rem;
        font-weight: 900;
        font-size: .72rem;
    }

    section[data-testid="stSidebar"] [role="radiogroup"] {
        background: rgba(255,255,255,0.72);
        border: 1px solid rgba(255,255,255,0.9);
        border-radius: 24px;
        padding: .85rem .9rem;
        box-shadow: inset 0 1px 0 rgba(255,255,255,.8), 0 16px 40px rgba(17,24,39,.07);
    }

    section[data-testid="stSidebar"] [data-testid="stRadio"] label {
        background: rgba(255,255,255,0.80);
        border: 1px solid rgba(17,24,39,0.08);
        border-radius: 18px;
        padding: .72rem .8rem;
        margin-bottom: .55rem;
        box-shadow: 0 10px 24px rgba(17,24,39,0.045);
    }

    section[data-testid="stSidebar"] [data-baseweb="select"] > div {
        background: rgba(255,255,255,0.92) !important;
        border: 1px solid rgba(17, 24, 39, 0.12) !important;
        border-radius: 18px !important;
        min-height: 3.1rem !important;
        box-shadow: 0 12px 28px rgba(17,24,39,0.055) !important;
    }

    section[data-testid="stSidebar"] [data-baseweb="select"] * {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    div[data-baseweb="popover"],
    div[data-baseweb="popover"] *,
    ul[role="listbox"],
    li[role="option"] {
        background-color: #ffffff !important;
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    li[role="option"]:hover {
        background-color: #f3f4f6 !important;
        color: var(--ink) !important;
    }

    .stButton {
        display: flex !important;
        justify-content: center !important;
        width: 100% !important;
        margin-top: 1rem !important;
        margin-bottom: 1rem !important;
    }

    .stButton>button {
        width: 96% !important;
        min-height: 3.35rem !important;
        border-radius: 999px !important;
        padding: .9rem 1.25rem !important;
        font-weight: 950 !important;
        font-size: 1rem !important;
        border: 1px solid rgba(17,24,39,.10) !important;
        background:
            linear-gradient(90deg, rgba(255,255,255,.92), rgba(237,233,254,.96), rgba(220,252,231,.92)) !important;
        color: var(--ink) !important;
        box-shadow: 0 18px 42px rgba(109, 40, 217, 0.18), 0 8px 18px rgba(22, 163, 74, 0.10) !important;
    }

    .stButton>button:hover {
        transform: translateY(-1px);
        box-shadow: 0 22px 48px rgba(109, 40, 217, 0.24), 0 10px 22px rgba(22, 163, 74, 0.12) !important;
    }

    .stButton>button p,
    .stButton>button span,
    .stButton>button div {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }

    .status-card {
        background: rgba(255,255,255,0.72);
        border: 1px solid rgba(255,255,255,0.9);
        border-radius: 22px;
        padding: .95rem 1rem;
        box-shadow: 0 14px 34px rgba(17,24,39,0.06);
        margin-top: 1rem;
    }

    .status-line {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: .5rem;
        padding: .34rem 0;
        border-bottom: 1px dashed rgba(17,24,39,.08);
        color: var(--muted);
        font-weight: 700;
        font-size: .88rem;
    }

    .status-line:last-child { border-bottom: none; }
    .status-ok { color: #15803d; font-weight: 950; }
    .status-bad { color: #b91c1c; font-weight: 950; }

    /* ========================= HERO ========================= */

    .hero-card {
        position: relative;
        margin: 1rem 0 1.7rem 0;
        padding: 3rem 3.1rem;
        border-radius: 34px;
        overflow: hidden;
        background:
            radial-gradient(circle at 12% 18%, rgba(255, 183, 77, 0.42), transparent 28%),
            radial-gradient(circle at 88% 16%, rgba(124, 58, 237, 0.24), transparent 32%),
            radial-gradient(circle at 76% 78%, rgba(20, 184, 166, 0.20), transparent 38%),
            rgba(255, 255, 255, 0.70);
        border: 1px solid rgba(255, 255, 255, 0.84);
        box-shadow: 0 24px 80px rgba(17, 24, 39, 0.10);
    }

    .hero-pill {
        display: inline-flex;
        gap: .5rem;
        align-items: center;
        padding: .72rem 1.1rem;
        border-radius: 999px;
        background: rgba(255, 255, 255, 0.78);
        border: 1px solid rgba(255, 255, 255, 0.9);
        font-size: .88rem;
        font-weight: 850;
        color: #20212a;
        margin-bottom: 2rem;
        box-shadow: 0 12px 32px rgba(17,24,39,.06);
    }

    .hero-title {
        max-width: 1120px;
        font-size: clamp(2.2rem, 4.0vw, 4.1rem);
        line-height: 1.03;
        letter-spacing: -0.065em;
        font-weight: 950;
        color: var(--ink);
        margin: 0;
    }

    .hero-purple {
        color: var(--purple) !important;
        -webkit-text-fill-color: var(--purple) !important;
        text-shadow: 0 14px 42px rgba(109, 40, 217, 0.12);
    }

    .hero-red {
        color: var(--red) !important;
        -webkit-text-fill-color: var(--red) !important;
        text-shadow: 0 12px 40px rgba(220, 38, 38, 0.14);
    }

    .hero-gradient {
        background: linear-gradient(90deg, #2563eb 0%, #0891b2 45%, #16a34a 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
    }

    .hero-subtitle {
        max-width: 860px;
        margin-top: 1.7rem;
        font-size: 1.08rem;
        line-height: 1.75;
        color: var(--muted);
        font-weight: 600;
    }

    .chip-row {
        display: flex;
        gap: .7rem;
        flex-wrap: wrap;
        margin-top: 1.7rem;
    }

    .chip {
        padding: .72rem 1rem;
        border-radius: 999px;
        background: rgba(255,255,255,.74);
        border: 1px solid rgba(255,255,255,.88);
        color: #252733;
        font-weight: 850;
        font-size: .9rem;
        box-shadow: 0 12px 32px rgba(17, 24, 39, 0.06);
    }

    /* ========================= CARDS + TABLES ========================= */

    .glass-card {
        background: rgba(255, 255, 255, 0.74);
        border: 1px solid rgba(255, 255, 255, 0.86);
        box-shadow: 0 20px 60px rgba(17, 24, 39, 0.08);
        border-radius: 28px;
        padding: 1.65rem;
        margin-bottom: 1rem;
    }

    .mini-label {
        color: #78716c;
        font-size: .75rem;
        text-transform: uppercase;
        letter-spacing: .17em;
        font-weight: 900;
        margin-bottom: .6rem;
    }

    .big-value {
        color: var(--ink);
        font-size: 2.1rem;
        font-weight: 950;
        line-height: 1.05;
        letter-spacing: -0.045em;
        margin-bottom: .5rem;
    }

    .small-note {
        color: var(--muted);
        line-height: 1.6;
        font-size: .95rem;
        font-weight: 600;
    }

    .section-title {
        margin: 1.8rem 0 1rem 0;
        color: var(--ink);
        font-size: 1.65rem;
        letter-spacing: -0.04em;
        font-weight: 950;
    }

    .map-shell {
        border-radius: 26px;
        overflow: hidden;
        border: 1px solid rgba(255,255,255,.88);
        box-shadow: 0 18px 60px rgba(15, 23, 42, 0.11);
        background: rgba(255,255,255,.76);
        padding: .7rem;
    }

    .legend-box {
        background: rgba(255,255,255,.74);
        border: 1px solid rgba(255,255,255,.88);
        border-radius: 22px;
        padding: 1.25rem 1.5rem;
        margin-bottom: 1rem;
        box-shadow: 0 16px 38px rgba(17,24,39,.05);
    }

    .legend-pill {
        display: inline-block;
        padding: .45rem .8rem;
        border-radius: 999px;
        margin: .22rem .25rem .22rem 0;
        font-weight: 900;
        font-size: .82rem;
    }

    .low { background:#dcfce7; color:#166534; }
    .watch { background:#fef3c7; color:#92400e; }
    .moderate { background:#ffedd5; color:#9a3412; }
    .high { background:#fee2e2; color:#991b1b; }
    .veryhigh { background:#f3e8ff; color:#6b21a8; }

    .alert-badge {
        display: inline-block;
        border-radius: 999px;
        padding: .35rem .7rem;
        font-size: .83rem;
        font-weight: 900;
    }

    .pretty-table-wrap {
        width: 100%;
        overflow-x: auto;
        border-radius: 24px;
        border: 1px solid rgba(17,24,39,.08);
        background: rgba(255,255,255,.74);
        box-shadow: 0 18px 52px rgba(17,24,39,.07);
        margin: 1rem 0 1.5rem 0;
    }

    table.pretty-table {
        width: 100%;
        border-collapse: collapse;
        color: var(--ink) !important;
        background: transparent;
        overflow: hidden;
    }

    table.pretty-table thead th {
        background: linear-gradient(90deg, rgba(237,233,254,.95), rgba(219,234,254,.95), rgba(220,252,231,.95));
        color: var(--ink) !important;
        font-weight: 950;
        text-align: left;
        padding: .95rem 1rem;
        font-size: .87rem;
        border-bottom: 1px solid rgba(17,24,39,.09);
        white-space: nowrap;
    }

    table.pretty-table tbody td {
        background: rgba(255,255,255,.70);
        color: var(--ink) !important;
        padding: .85rem 1rem;
        font-size: .9rem;
        font-weight: 650;
        border-bottom: 1px solid rgba(17,24,39,.065);
        vertical-align: top;
    }

    table.pretty-table tbody tr:nth-child(even) td {
        background: rgba(248,250,252,.78);
    }

    table.pretty-table tbody tr:hover td {
        background: rgba(254,243,199,.62);
    }

    .footer-note {
        margin-top: 2rem;
        padding: 1.2rem;
        text-align: center;
        color: var(--soft-muted);
        font-size: .9rem;
        font-weight: 650;
    }

    [data-testid="stMetricValue"] {
        color: var(--ink) !important;
        font-weight: 900 !important;
        letter-spacing: -0.045em !important;
    }

    [data-testid="stMetricLabel"] {
        color: #374151 !important;
        font-weight: 750 !important;
    }

    div[data-testid="stDataFrame"] * {
        color: var(--ink) !important;
        -webkit-text-fill-color: var(--ink) !important;
    }


    /* ========================= CUSTOM LOADING CARD ========================= */

    .forecast-loading-card {
        position: relative;
        overflow: hidden;
        border-radius: 30px;
        padding: 1.45rem 1.6rem;
        margin: 1.2rem 0 1.4rem 0;
        background:
            radial-gradient(circle at 10% 20%, rgba(255, 183, 77, 0.32), transparent 30%),
            radial-gradient(circle at 86% 20%, rgba(109, 40, 217, 0.22), transparent 34%),
            radial-gradient(circle at 70% 85%, rgba(20, 184, 166, 0.18), transparent 34%),
            rgba(255, 255, 255, 0.78);
        border: 1px solid rgba(255, 255, 255, 0.92);
        box-shadow: 0 22px 70px rgba(17, 24, 39, 0.10);
    }

    .forecast-loading-card:before {
        content: "";
        position: absolute;
        inset: -80px;
        background: conic-gradient(from 180deg, rgba(109,40,217,.0), rgba(109,40,217,.18), rgba(37,99,235,.18), rgba(20,184,166,.16), rgba(109,40,217,.0));
        animation: forecast-spin 4.2s linear infinite;
        z-index: 0;
    }

    .forecast-loading-card:after {
        content: "";
        position: absolute;
        inset: 2px;
        border-radius: 28px;
        background: rgba(255, 255, 255, 0.72);
        backdrop-filter: blur(18px);
        z-index: 1;
    }

    @keyframes forecast-spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
    }

    .forecast-loading-content {
        position: relative;
        z-index: 2;
        display: grid;
        grid-template-columns: auto 1fr;
        gap: 1rem;
        align-items: center;
    }

    .forecast-orb {
        width: 58px;
        height: 58px;
        border-radius: 22px;
        display: grid;
        place-items: center;
        font-size: 1.55rem;
        background: linear-gradient(135deg, #ede9fe, #dbeafe, #dcfce7);
        border: 1px solid rgba(255, 255, 255, 0.92);
        box-shadow: 0 12px 30px rgba(109, 40, 217, 0.16);
        animation: forecast-pulse 1.5s ease-in-out infinite;
    }

    @keyframes forecast-pulse {
        0%, 100% { transform: scale(1); filter: saturate(1); }
        50% { transform: scale(1.06); filter: saturate(1.25); }
    }

    .forecast-loading-title {
        color: var(--ink);
        font-size: 1.2rem;
        font-weight: 950;
        letter-spacing: -0.035em;
        margin-bottom: .25rem;
    }

    .forecast-loading-subtitle {
        color: var(--muted);
        font-weight: 650;
        line-height: 1.45;
        font-size: .95rem;
    }

    .forecast-loading-chips {
        position: relative;
        z-index: 2;
        margin-top: 1rem;
        display: flex;
        flex-wrap: wrap;
        gap: .55rem;
    }

    .forecast-loading-chip {
        border-radius: 999px;
        padding: .48rem .78rem;
        background: rgba(255, 255, 255, 0.82);
        border: 1px solid rgba(17, 24, 39, 0.08);
        color: var(--ink);
        font-size: .78rem;
        font-weight: 900;
        box-shadow: 0 8px 22px rgba(17,24,39,.055);
    }

    div[data-testid="stProgress"] > div {
        background: rgba(17, 24, 39, 0.08) !important;
        border-radius: 999px !important;
        height: 12px !important;
        overflow: hidden !important;
    }

    div[data-testid="stProgress"] > div > div {
        background: linear-gradient(90deg, #6d28d9, #2563eb, #14b8a6, #16a34a) !important;
        border-radius: 999px !important;
    }





    /* ========================= FULL-SCREEN LOADING OVERLAY ========================= */

    .forecast-loading-overlay {
        position: fixed;
        inset: 0;
        z-index: 999999;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 2rem;
        background:
            radial-gradient(circle at 18% 18%, rgba(255, 202, 138, 0.70), transparent 32%),
            radial-gradient(circle at 84% 18%, rgba(124, 58, 237, 0.34), transparent 35%),
            radial-gradient(circle at 72% 82%, rgba(20, 184, 166, 0.24), transparent 36%),
            linear-gradient(135deg, rgba(255, 247, 237, 0.94), rgba(248, 250, 252, 0.94), rgba(238, 242, 255, 0.94));
        backdrop-filter: blur(20px);
    }

    .forecast-loading-overlay-card {
        position: relative;
        overflow: hidden;
        width: min(820px, 92vw);
        border-radius: 38px;
        padding: 2.4rem 2.2rem;
        background:
            radial-gradient(circle at 12% 20%, rgba(255, 183, 77, 0.26), transparent 34%),
            radial-gradient(circle at 88% 16%, rgba(109, 40, 217, 0.20), transparent 34%),
            radial-gradient(circle at 72% 88%, rgba(20, 184, 166, 0.18), transparent 36%),
            rgba(255, 255, 255, 0.88);
        border: 1px solid rgba(255, 255, 255, 0.96);
        box-shadow: 0 34px 100px rgba(17, 24, 39, 0.20);
        text-align: center;
    }

    .forecast-loading-overlay-card:before {
        content: "";
        position: absolute;
        inset: -120px;
        background: conic-gradient(from 180deg, rgba(220,38,38,0), rgba(220,38,38,.13), rgba(109,40,217,.18), rgba(37,99,235,.16), rgba(20,184,166,.15), rgba(220,38,38,0));
        animation: forecast-overlay-spin 4.8s linear infinite;
        z-index: 0;
    }

    .forecast-loading-overlay-card:after {
        content: "";
        position: absolute;
        inset: 3px;
        border-radius: 35px;
        background: rgba(255, 255, 255, 0.76);
        backdrop-filter: blur(18px);
        z-index: 1;
    }

    @keyframes forecast-overlay-spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
    }

    .forecast-loading-overlay-content {
        position: relative;
        z-index: 2;
    }

    .forecast-loading-orb-wrap {
        display: flex;
        justify-content: center;
        margin-bottom: 1.25rem;
    }

    .forecast-loading-orb {
        width: 96px;
        height: 96px;
        border-radius: 32px;
        display: grid;
        place-items: center;
        font-size: 2.05rem;
        background: linear-gradient(135deg, #fee2e2, #ede9fe, #dbeafe, #dcfce7);
        border: 1px solid rgba(255, 255, 255, 0.98);
        box-shadow: 0 18px 52px rgba(109, 40, 217, 0.20);
        animation: forecast-overlay-pulse 1.25s ease-in-out infinite;
    }

    @keyframes forecast-overlay-pulse {
        0%, 100% { transform: translateY(0) scale(1); filter: saturate(1); }
        50% { transform: translateY(-4px) scale(1.045); filter: saturate(1.25); }
    }

    .forecast-loading-overlay-title {
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
        font-size: clamp(1.75rem, 3vw, 2.65rem);
        line-height: 1.05;
        letter-spacing: -0.06em;
        font-weight: 950;
        margin-bottom: .75rem;
    }

    .forecast-loading-overlay-subtitle {
        color: #4b5563 !important;
        -webkit-text-fill-color: #4b5563 !important;
        max-width: 650px;
        margin: 0 auto 1.35rem auto;
        font-size: 1.02rem;
        line-height: 1.65;
        font-weight: 650;
    }

    .forecast-loading-overlay-steps {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: .78rem;
        margin-top: 1rem;
    }

    .forecast-loading-overlay-step {
        padding: .9rem .85rem;
        border-radius: 20px;
        background: linear-gradient(135deg, rgba(255,255,255,0.94), rgba(240,253,244,0.84));
        border: 1px solid rgba(17, 24, 39, 0.07);
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
        font-weight: 900;
        font-size: .88rem;
        box-shadow: 0 12px 26px rgba(17,24,39,0.055);
    }

    .forecast-loading-overlay-progress {
        height: 13px;
        border-radius: 999px;
        background: rgba(17, 24, 39, 0.08);
        overflow: hidden;
        margin-top: 1.45rem;
    }

    .forecast-loading-overlay-progress-bar {
        height: 100%;
        width: 38%;
        border-radius: 999px;
        background: linear-gradient(90deg, #dc2626, #7c3aed, #2563eb, #14b8a6, #16a34a);
        animation: forecast-overlay-slide 1.15s ease-in-out infinite alternate;
    }

    @keyframes forecast-overlay-slide {
        0% { transform: translateX(-25%); width: 35%; }
        100% { transform: translateX(175%); width: 46%; }
    }

    @media (max-width: 850px) {
        .forecast-loading-overlay-steps {
            grid-template-columns: 1fr;
        }
    }


    /* ========================= NO WHITE TEXT SAFETY PATCH =========================
       Keeps every normal Streamlit/app label dark and readable. Special hero/alert
       classes are restored immediately after this block. */
    .stApp p,
    .stApp div,
    .stApp span,
    .stApp label,
    .stApp li,
    .stApp td,
    .stApp th,
    .stApp button,
    .stApp [data-testid="stMarkdownContainer"],
    .stApp [data-testid="stMarkdownContainer"] *,
    .stApp [data-testid="stMetricLabel"],
    .stApp [data-testid="stMetricValue"],
    .stApp [data-testid="stMetricDelta"],
    .stApp [data-testid="stWidgetLabel"],
    .stApp [data-baseweb="radio"] *,
    .stApp [data-baseweb="select"] *,
    section[data-testid="stSidebar"] * {
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
    }

    /* Restore intentional hero colors after the safety patch. */
    .hero-purple,
    .hero-purple * {
        color: #6d28d9 !important;
        -webkit-text-fill-color: #6d28d9 !important;
    }

    .hero-red,
    .hero-red * {
        color: #dc2626 !important;
        -webkit-text-fill-color: #dc2626 !important;
    }

    .hero-gradient,
    .hero-gradient * {
        background: linear-gradient(90deg, #2563eb 0%, #16a34a 100%) !important;
        -webkit-background-clip: text !important;
        background-clip: text !important;
        color: transparent !important;
        -webkit-text-fill-color: transparent !important;
    }

    /* Restore alert/legend readable colors. */
    .low { background:#dcfce7 !important; color:#166534 !important; -webkit-text-fill-color:#166534 !important; }
    .watch { background:#fef3c7 !important; color:#92400e !important; -webkit-text-fill-color:#92400e !important; }
    .moderate { background:#ffedd5 !important; color:#9a3412 !important; -webkit-text-fill-color:#9a3412 !important; }
    .high { background:#fee2e2 !important; color:#991b1b !important; -webkit-text-fill-color:#991b1b !important; }
    .veryhigh { background:#f3e8ff !important; color:#6b21a8 !important; -webkit-text-fill-color:#6b21a8 !important; }

    /* Keep custom light tables readable, not black/white. */
    .pretty-table-wrap,
    .pretty-table,
    .pretty-table thead,
    .pretty-table tbody,
    .pretty-table tr,
    .pretty-table th,
    .pretty-table td {
        background: transparent !important;
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
    }

    .pretty-table th {
        background: linear-gradient(90deg, rgba(237,233,254,.92), rgba(220,252,231,.85)) !important;
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
    }

    .pretty-table td {
        background: rgba(255,255,255,.74) !important;
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
    }

    /* Loading card text must stay dark. */
    .loading-card,
    .loading-card *,
    .loading-step,
    .loading-step * {
        color: #111827 !important;
        -webkit-text-fill-color: #111827 !important;
    }

    </style>
    """,
    unsafe_allow_html=True,
)


# ============================================================
# BASIC HELPERS
# ============================================================

def standardize_barangay_name(value: Any) -> str:
    return str(value).upper().strip()


def clean_alert(alert: Any) -> str:
    return str(alert).lower().replace("_", " ").strip()


def alert_rank(alert: Any) -> int:
    alert_clean = clean_alert(alert)
    ranks = {
        "low": 1,
        "watch": 2,
        "warning": 2,
        "moderate": 3,
        "high": 4,
        "very high": 5,
    }
    return ranks.get(alert_clean, 0)


def alert_color(alert: Any) -> str:
    alert_clean = clean_alert(alert)

    if alert_clean == "low":
        return "#22c55e"
    if alert_clean in ["watch", "warning"]:
        return "#facc15"
    if alert_clean == "moderate":
        return "#fb923c"
    if alert_clean == "high":
        return "#ef4444"
    if alert_clean == "very high":
        return "#a855f7"

    return "#cbd5e1"


def alert_badge_class(alert: Any) -> str:
    alert_clean = clean_alert(alert)

    if alert_clean == "low":
        return "low"
    if alert_clean in ["watch", "warning"]:
        return "watch"
    if alert_clean == "moderate":
        return "moderate"
    if alert_clean == "high":
        return "high"
    if alert_clean == "very high":
        return "veryhigh"

    return "watch"


def fmt_number(value: Any, digits: int = 1) -> str:
    try:
        if value is None or pd.isna(value):
            return "—"
        return f"{float(value):,.{digits}f}"
    except Exception:
        return "—"


def fmt_percent(value: Any, digits: int = 1) -> str:
    try:
        if value is None or pd.isna(value):
            return "—"
        return f"{float(value) * 100:.{digits}f}%"
    except Exception:
        return "—"


def make_prediction_range(predicted_cases: Any, error_value: Any) -> str:
    try:
        if predicted_cases is None or pd.isna(predicted_cases):
            return "Not available"

        pred = float(predicted_cases)

        if error_value is None or pd.isna(error_value):
            rounded = int(round(max(0, pred)))
            return f"{rounded} case(s)"

        err = abs(float(error_value))

        lower = max(0, pred - err)
        upper = max(0, pred + err)

        lower_i = int(round(lower))
        upper_i = int(round(upper))

        if lower_i == upper_i:
            return f"{lower_i} case(s)"

        return f"{lower_i}–{upper_i} cases"

    except Exception:
        return "Not available"


def horizon_label(horizon_result: Dict[str, Any]) -> str:
    h = int(horizon_result.get("horizon", horizon_result.get("forecast_horizon", 0)))

    labels = {
        0: "Selected week",
        1: "1 week after",
        2: "2 weeks after",
        3: "3 weeks after",
        4: "4 weeks after",
        8: "8 weeks after",
        12: "12 weeks after",
    }

    return labels.get(h, f"{h} weeks after")


def horizon_sort_key(horizon_result: Dict[str, Any]) -> int:
    try:
        return int(horizon_result.get("horizon", horizon_result.get("forecast_horizon", 999)))
    except Exception:
        return 999


def ensure_probability(row: Dict[str, Any]) -> Optional[float]:
    for key in [
        "outbreak_probability",
        "estimated_outbreak_probability",
        "probability",
        "predicted_risk",
        "mean_outbreak_probability",
    ]:
        if key in row and row[key] is not None:
            try:
                value = float(row[key])
                return min(max(value, 0.0), 1.0)
            except Exception:
                pass

    return None


def get_intervention_plan(alert_level: Any) -> List[str]:
    alert = clean_alert(alert_level)

    if "above outbreak" in alert:
        alert = "high"
    elif "below outbreak" in alert:
        alert = "low"

    if alert == "low":
        return [
            "Continue routine weekly dengue monitoring.",
            "Maintain regular clean-up activities.",
            "Remind households to remove standing water and cover water containers.",
        ]

    if alert in ["watch", "warning"]:
        return [
            "Increase dengue information reminders in the barangay.",
            "Inspect common mosquito breeding sites.",
            "Coordinate clean-up reminders with purok or community leaders.",
        ]

    if alert == "moderate":
        return [
            "Conduct targeted source reduction in high-risk sitios.",
            "Inspect schools, markets, drainage areas, and construction sites.",
            "Prepare health-center monitoring for possible increases in dengue cases.",
        ]

    if alert == "high":
        return [
            "Prioritize barangay vector-control operations.",
            "Intensify larval source reduction and household clean-up checks.",
            "Coordinate with city health staff for focused hotspot response.",
        ]

    if alert == "very high":
        return [
            "Activate urgent barangay dengue response.",
            "Deploy intensified vector surveillance and source reduction.",
            "Coordinate with city health authorities for outbreak response planning.",
        ]

    return [
        "Review the forecast output and verify available case and environmental data.",
        "Continue routine surveillance and community dengue prevention reminders.",
    ]


def interventions_to_html(items: List[str]) -> str:
    safe_items = [html.escape(str(item)) for item in items]
    return "<ul>" + "".join([f"<li>{item}</li>" for item in safe_items]) + "</ul>"


def render_alert_legend() -> None:
    st.markdown(
        """
        <div class="legend-box">
            <div class="mini-label">Map legend</div>
            <span class="legend-pill low">Low</span>
            <span class="legend-pill watch">Watch</span>
            <span class="legend-pill moderate">Moderate</span>
            <span class="legend-pill high">High</span>
            <span class="legend-pill veryhigh">Very high</span>
            <div class="small-note" style="margin-top:.55rem;">
                Colors represent dengue alert levels generated from model outputs.
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )


def render_metric_card(title: str, value: str, note: str) -> None:
    st.markdown(
        f"""
        <div class="glass-card">
            <div class="mini-label">{html.escape(title)}</div>
            <div class="big-value">{html.escape(value)}</div>
            <div class="small-note">{html.escape(note)}</div>
        </div>
        """,
        unsafe_allow_html=True,
    )


def render_pretty_table(df: pd.DataFrame, max_rows: int = 120) -> None:
    if df.empty:
        st.info("No rows to display.")
        return

    display_df = df.head(max_rows).copy()

    header_html = "".join(
        f"<th>{html.escape(str(col))}</th>" for col in display_df.columns
    )

    body_rows = []
    for _, row in display_df.iterrows():
        cells = "".join(
            f"<td>{html.escape(str(row[col]))}</td>" for col in display_df.columns
        )
        body_rows.append(f"<tr>{cells}</tr>")

    caption = ""
    if len(df) > max_rows:
        caption = f"<div class='small-note' style='padding:.85rem 1rem;'>Showing first {max_rows} of {len(df)} rows.</div>"

    st.markdown(
        f"""
        <div class="pretty-table-wrap">
            <table class="pretty-table">
                <thead><tr>{header_html}</tr></thead>
                <tbody>{''.join(body_rows)}</tbody>
            </table>
            {caption}
        </div>
        """,
        unsafe_allow_html=True,
    )


# ============================================================
# R BRIDGE HELPERS
# ============================================================

def find_rscript() -> str:
    """Find Rscript on local Windows, Linux, Streamlit Cloud, or Posit-like servers."""
    found = shutil.which("Rscript")
    if found:
        return found

    candidates = []

    # Windows local paths
    candidates.extend(glob.glob(r"C:\Program Files\R\R-*\bin\x64\Rscript.exe"))
    candidates.extend(glob.glob(r"C:\Program Files\R\R-*\bin\Rscript.exe"))
    candidates.extend(glob.glob(r"C:\Program Files (x86)\R\R-*\bin\x64\Rscript.exe"))
    candidates.extend(glob.glob(r"C:\Program Files (x86)\R\R-*\bin\Rscript.exe"))

    # Linux / cloud paths
    candidates.extend(glob.glob("/usr/bin/Rscript"))
    candidates.extend(glob.glob("/usr/local/bin/Rscript"))
    candidates.extend(glob.glob("/opt/R/*/bin/Rscript"))
    candidates.extend(glob.glob("/opt/rstudio-connect/mnt/R/*/bin/Rscript"))

    candidates = [c for c in candidates if Path(c).exists()]

    if candidates:
        return sorted(candidates, reverse=True)[0]

    raise FileNotFoundError(
        "Rscript could not be found. This app needs R installed because it calls "
        "r_scripts/predict_on_demand.R. Install R / r-base, or make Rscript available on PATH."
    )


@st.cache_resource(show_spinner=False)
def ensure_r_packages_ready() -> bool:
    """
    Optional deployment helper.
    If r_packages_setup.R exists, run it once per app session to prepare R packages.
    If it does not exist, skip this step and let predict_on_demand.R check packages.
    """
    if not R_PACKAGE_SETUP_PATH.exists():
        return True

    rscript = find_rscript()

    result = subprocess.run(
        [rscript, str(R_PACKAGE_SETUP_PATH)],
        cwd=str(BASE_DIR),
        capture_output=True,
        text=True,
        shell=False,
    )

    combined_output = ""
    if result.stdout:
        combined_output += result.stdout
    if result.stderr:
        combined_output += "\n" + result.stderr

    if result.returncode != 0:
        raise RuntimeError(combined_output.strip())

    return True

def extract_json_object(text: str) -> Dict[str, Any]:
    if not text:
        raise ValueError("No output was returned by the R prediction script.")

    start = text.find("{")
    if start == -1:
        raise ValueError(f"No JSON object found in R output:\n{text}")

    depth = 0
    in_string = False
    escape = False

    for i in range(start, len(text)):
        char = text[i]

        if in_string:
            if escape:
                escape = False
            elif char == "\\":
                escape = True
            elif char == '"':
                in_string = False
        else:
            if char == '"':
                in_string = True
            elif char == "{":
                depth += 1
            elif char == "}":
                depth -= 1
                if depth == 0:
                    json_text = text[start:i + 1]
                    return json.loads(json_text)

    raise ValueError(f"Incomplete JSON object in R output:\n{text}")


def run_r_prediction(mode: str, level: str, year: int, week: int) -> Dict[str, Any]:
    if not R_SCRIPT_PATH.exists():
        raise FileNotFoundError(
            f"Missing R bridge file: {R_SCRIPT_PATH}\n"
            "Make sure predict_on_demand.R is inside the r_scripts folder."
        )

    rscript = find_rscript()

    command = [
        rscript,
        str(R_SCRIPT_PATH),
        "--mode",
        mode,
        "--level",
        level,
        "--year",
        str(year),
        "--week",
        str(week),
    ]

    result = subprocess.run(
        command,
        cwd=str(BASE_DIR),
        capture_output=True,
        text=True,
        shell=False,
    )

    combined_output = ""
    if result.stdout:
        combined_output += result.stdout
    if result.stderr:
        combined_output += "\n" + result.stderr

    if result.returncode != 0:
        raise RuntimeError(combined_output.strip())

    return extract_json_object(combined_output)


# ============================================================
# DATA LOADERS
# ============================================================

@st.cache_data(show_spinner=False)
def load_dataset_weeks() -> pd.DataFrame:
    if not DATASET_PATH.exists():
        return pd.DataFrame(columns=["year", "week"])

    df = pd.read_excel(DATASET_PATH)
    df.columns = [str(c).strip().lower().replace(" ", "_") for c in df.columns]

    if "year" not in df.columns or "week" not in df.columns:
        return pd.DataFrame(columns=["year", "week"])

    out = df[["year", "week"]].dropna().copy()
    out["year"] = out["year"].astype(int)
    out["week"] = out["week"].astype(int)

    return out.drop_duplicates().sort_values(["year", "week"])


@st.cache_data(show_spinner=False)
def load_barangay_shapefile() -> Optional[gpd.GeoDataFrame]:
    if not SHAPE_ZIP_PATH.exists():
        return None

    with tempfile.TemporaryDirectory() as tmpdir:
        with zipfile.ZipFile(SHAPE_ZIP_PATH, "r") as zip_ref:
            zip_ref.extractall(tmpdir)

        shp_files = list(Path(tmpdir).glob("*.shp"))

        if not shp_files:
            return None

        gdf = gpd.read_file(shp_files[0])
        gdf.columns = [str(c).lower().strip() for c in gdf.columns]

        possible_name_columns = [
            "barangay",
            "brgy",
            "name",
            "adm4_en",
            "adm4_name",
            "bgy_name",
            "barangay_n",
            "brgy_name",
        ]

        name_col = None
        for col in possible_name_columns:
            if col in gdf.columns:
                name_col = col
                break

        if name_col is None:
            return None

        gdf["barangay"] = gdf[name_col].apply(standardize_barangay_name)

        if gdf.crs is None:
            gdf = gdf.set_crs(epsg=4326, allow_override=True)
        else:
            gdf = gdf.to_crs(epsg=4326)

        return gdf[["barangay", "geometry"]].copy()


# ============================================================
# MAP HELPERS
# ============================================================

def create_barangay_prediction_map(
    shape_gdf: gpd.GeoDataFrame,
    barangay_rows: pd.DataFrame,
) -> folium.Map:
    map_gdf = shape_gdf.merge(barangay_rows, on="barangay", how="left")

    fill_defaults = {
        "predicted_cases": 0,
        "predicted_cases_display": "0.0",
        "probability_display": "—",
        "alert_level": "Unknown",
        "predicted_case_range": "Not available",
        "range_basis": "Predicted cases ± MAE",
        "intervention_html": "<ul><li>No recommendation available.</li></ul>",
        "target_period": "—",
    }

    for col, val in fill_defaults.items():
        if col not in map_gdf.columns:
            map_gdf[col] = val
        else:
            map_gdf[col] = map_gdf[col].fillna(val)

    m = folium.Map(
        location=[10.3157, 123.8854],
        zoom_start=11,
        tiles="CartoDB positron",
        control_scale=True,
    )

    def style_function(feature):
        alert = feature["properties"].get("alert_level", "Unknown")
        return {
            "fillColor": alert_color(alert),
            "color": "#ffffff",
            "weight": 1,
            "fillOpacity": 0.74,
        }

    def highlight_function(feature):
        return {
            "color": "#111827",
            "weight": 3,
            "fillOpacity": 0.90,
        }

    tooltip = folium.GeoJsonTooltip(
        fields=[
            "barangay",
            "predicted_case_range",
            "probability_display",
            "alert_level",
        ],
        aliases=[
            "Barangay:",
            "Predicted case range:",
            "Outbreak probability:",
            "Alert level:",
        ],
        sticky=True,
        style=(
            "background-color: rgba(255,255,255,0.96); color: #111827; "
            "font-family: Inter, Arial; font-size: 13px; padding: 10px; "
            "border-radius: 12px; box-shadow: 0 10px 30px rgba(0,0,0,0.12);"
        ),
    )

    popup = folium.GeoJsonPopup(
        fields=[
            "barangay",
            "target_period",
            "predicted_case_range",
            "range_basis",
            "probability_display",
            "alert_level",
            "intervention_html",
        ],
        aliases=[
            "Barangay:",
            "Forecast period:",
            "Predicted case range:",
            "Range based on:",
            "Outbreak probability:",
            "Alert level:",
            "Recommended interventions:",
        ],
        localize=True,
        labels=True,
        max_width=430,
    )

    folium.GeoJson(
        map_gdf,
        name="Barangay dengue forecast",
        style_function=style_function,
        highlight_function=highlight_function,
        popup=popup,
        tooltip=tooltip,
    ).add_to(m)

    return m


# ============================================================
# RESULT PREPARATION
# ============================================================

def prepare_barangay_rows(horizon_result: Dict[str, Any]) -> pd.DataFrame:
    rows = horizon_result.get("barangay_predictions", []) or horizon_result.get("barangays", []) or []
    df = pd.DataFrame(rows)

    if df.empty:
        return df

    if "barangay" not in df.columns:
        return pd.DataFrame()

    df["barangay"] = df["barangay"].apply(standardize_barangay_name)

    if "predicted_cases" not in df.columns:
        for candidate in [
            "soft_gated_xgboost_cases",
            "environmental_xgboost_cases",
            "environmental_lightgbm_cases",
        ]:
            if candidate in df.columns:
                df["predicted_cases"] = df[candidate]
                break

    if "predicted_cases" not in df.columns:
        df["predicted_cases"] = 0.0

    if "alert_level" not in df.columns:
        df["alert_level"] = "Unknown"

    df["probability"] = df.apply(lambda r: ensure_probability(r.to_dict()), axis=1)
    df["predicted_cases"] = pd.to_numeric(df["predicted_cases"], errors="coerce").fillna(0)
    df["predicted_cases_display"] = df["predicted_cases"].apply(lambda x: fmt_number(x, 2))
    df["probability_display"] = df["probability"].apply(lambda x: fmt_percent(x, 1))

    error_value = (
        horizon_result.get("mae")
        or horizon_result.get("MAE_raw")
        or horizon_result.get("error_margin")
    )

    df["predicted_case_range"] = df["predicted_cases"].apply(
        lambda x: make_prediction_range(x, error_value)
    )

    df["range_basis"] = "Predicted cases ± MAE"

    target_year = horizon_result.get("target_year", "—")
    target_week = horizon_result.get("target_week", "—")
    df["target_period"] = f"{horizon_label(horizon_result)} · {target_year} W{target_week}"

    df["intervention_html"] = df["alert_level"].apply(
        lambda a: interventions_to_html(get_intervention_plan(a))
    )

    df["alert_rank"] = df["alert_level"].apply(alert_rank)

    return df


def city_case_range(horizon_result: Dict[str, Any]) -> str:
    pred_cases = horizon_result.get("predicted_cases") or horizon_result.get("total_predicted_cases")

    error_value = (
        horizon_result.get("mae")
        or horizon_result.get("MAE_raw")
        or horizon_result.get("error_margin")
    )

    return make_prediction_range(pred_cases, error_value)


# ============================================================
# HERO
# ============================================================

def hero() -> None:
    st.markdown(
        """
        <div class="hero-card">
            <div class="hero-pill">🦟 Cebu City Dengue Forecasting App</div>
            <h1 class="hero-title">
                <span class="hero-purple">Predict</span>
                <span class="hero-red">dengue cases</span>
                <span class="hero-purple">with a</span>
                <span class="hero-gradient">clear weekly alert system.</span>
            </h1>
            <div class="hero-subtitle">
                Select a target week, choose the prediction mode, and generate both citywide
                and barangay-level dengue forecasts for the selected week and future planning windows.
            </div>
            <div class="chip-row">
                <div class="chip">Barangay shapefile map</div>
                <div class="chip">Citywide forecast</div>
                <div class="chip">Predicted case ranges</div>
                <div class="chip">Alert levels</div>
                <div class="chip">Bullet-point interventions</div>
                <div class="chip">No graph clutter</div>
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )


# ============================================================
# FULL BROWSER LOADING OVERLAY
# ============================================================

def show_forecast_loading_overlay() -> None:
    """Show a true full-page browser overlay using a small JS injection."""
    components.html(
        """
        <script>
        (function() {
            const doc = window.parent.document;

            const oldOverlay = doc.getElementById("forecast-loading-overlay");
            if (oldOverlay) oldOverlay.remove();

            const oldStyle = doc.getElementById("forecast-loading-overlay-style");
            if (oldStyle) oldStyle.remove();

            const style = doc.createElement("style");
            style.id = "forecast-loading-overlay-style";
            style.innerHTML = `
                #forecast-loading-overlay {
                    position: fixed;
                    inset: 0;
                    z-index: 2147483647;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    padding: 2rem;
                    background:
                        radial-gradient(circle at 16% 16%, rgba(251, 191, 36, 0.38), transparent 32%),
                        radial-gradient(circle at 84% 18%, rgba(124, 58, 237, 0.34), transparent 35%),
                        radial-gradient(circle at 72% 84%, rgba(20, 184, 166, 0.25), transparent 37%),
                        linear-gradient(135deg, rgba(255, 247, 237, 0.92), rgba(248, 250, 252, 0.94), rgba(238, 242, 255, 0.92));
                    backdrop-filter: blur(20px);
                    font-family: Inter, Arial, sans-serif;
                }

                #forecast-loading-card {
                    position: relative;
                    overflow: hidden;
                    width: min(820px, 92vw);
                    border-radius: 38px;
                    padding: 2.5rem 2.25rem;
                    background:
                        radial-gradient(circle at 12% 20%, rgba(255, 183, 77, 0.27), transparent 35%),
                        radial-gradient(circle at 88% 18%, rgba(109, 40, 217, 0.20), transparent 36%),
                        radial-gradient(circle at 72% 88%, rgba(20, 184, 166, 0.18), transparent 38%),
                        rgba(255, 255, 255, 0.90);
                    border: 1px solid rgba(255, 255, 255, 0.96);
                    box-shadow: 0 34px 100px rgba(17, 24, 39, 0.22);
                    text-align: center;
                }

                #forecast-loading-card::before {
                    content: "";
                    position: absolute;
                    inset: -140px;
                    background: conic-gradient(from 180deg, rgba(220,38,38,0), rgba(220,38,38,.13), rgba(109,40,217,.19), rgba(37,99,235,.17), rgba(20,184,166,.16), rgba(220,38,38,0));
                    animation: forecastOverlaySpin 4.8s linear infinite;
                    z-index: 0;
                }

                #forecast-loading-card::after {
                    content: "";
                    position: absolute;
                    inset: 3px;
                    border-radius: 35px;
                    background: rgba(255, 255, 255, 0.78);
                    backdrop-filter: blur(18px);
                    z-index: 1;
                }

                #forecast-loading-content {
                    position: relative;
                    z-index: 2;
                }

                @keyframes forecastOverlaySpin {
                    from { transform: rotate(0deg); }
                    to { transform: rotate(360deg); }
                }

                #forecast-loading-orb {
                    width: 102px;
                    height: 102px;
                    margin: 0 auto 1.35rem auto;
                    border-radius: 32px;
                    display: grid;
                    place-items: center;
                    font-size: 2.35rem;
                    background: linear-gradient(135deg, #fee2e2, #ede9fe, #dbeafe, #dcfce7);
                    border: 1px solid rgba(255, 255, 255, 0.98);
                    box-shadow: 0 18px 52px rgba(109, 40, 217, 0.22);
                    animation: forecastOverlayPulse 1.25s ease-in-out infinite;
                }

                @keyframes forecastOverlayPulse {
                    0%, 100% { transform: translateY(0) scale(1); filter: saturate(1); }
                    50% { transform: translateY(-4px) scale(1.045); filter: saturate(1.25); }
                }

                #forecast-loading-title {
                    color: #111827;
                    font-size: clamp(1.8rem, 3vw, 2.65rem);
                    line-height: 1.05;
                    letter-spacing: -0.06em;
                    font-weight: 950;
                    margin-bottom: .75rem;
                }

                #forecast-loading-subtitle {
                    color: #374151;
                    max-width: 650px;
                    margin: 0 auto 1.35rem auto;
                    font-size: 1.02rem;
                    line-height: 1.65;
                    font-weight: 650;
                }

                #forecast-loading-steps {
                    display: grid;
                    grid-template-columns: repeat(3, 1fr);
                    gap: .78rem;
                    margin-top: 1rem;
                }

                .forecast-loading-step {
                    padding: .92rem .88rem;
                    border-radius: 20px;
                    background: linear-gradient(135deg, rgba(255,255,255,0.94), rgba(240,253,244,0.84));
                    border: 1px solid rgba(17, 24, 39, 0.07);
                    color: #111827;
                    font-weight: 900;
                    font-size: .9rem;
                    box-shadow: 0 12px 26px rgba(17,24,39,0.055);
                }

                #forecast-loading-progress {
                    height: 13px;
                    border-radius: 999px;
                    background: rgba(17, 24, 39, 0.08);
                    overflow: hidden;
                    margin-top: 1.5rem;
                }

                #forecast-loading-progress-bar {
                    height: 100%;
                    width: 38%;
                    border-radius: 999px;
                    background: linear-gradient(90deg, #dc2626, #7c3aed, #2563eb, #14b8a6, #16a34a);
                    animation: forecastOverlaySlide 1.15s ease-in-out infinite alternate;
                }

                @keyframes forecastOverlaySlide {
                    0% { transform: translateX(-25%); width: 35%; }
                    100% { transform: translateX(175%); width: 46%; }
                }

                @media (max-width: 850px) {
                    #forecast-loading-steps { grid-template-columns: 1fr; }
                    #forecast-loading-title { font-size: 1.65rem; }
                }
            `;

            doc.head.appendChild(style);

            const overlay = doc.createElement("div");
            overlay.id = "forecast-loading-overlay";
            overlay.innerHTML = `
                <div id="forecast-loading-card">
                    <div id="forecast-loading-content">
                        <div id="forecast-loading-orb">🦟</div>
                        <div id="forecast-loading-title">Launching dengue forecast engine</div>
                        <div id="forecast-loading-subtitle">
                            Checking the selected week, loading saved models, generating citywide forecasts,
                            preparing the barangay risk map, and finalizing weekly alert results.
                        </div>
                        <div id="forecast-loading-steps">
                            <div class="forecast-loading-step">Citywide model running</div>
                            <div class="forecast-loading-step">Barangay risk map preparing</div>
                            <div class="forecast-loading-step">Weekly alerts finalizing</div>
                        </div>
                        <div id="forecast-loading-progress">
                            <div id="forecast-loading-progress-bar"></div>
                        </div>
                    </div>
                </div>
            `;

            doc.body.appendChild(overlay);
        })();
        </script>
        """,
        height=0,
        width=0,
    )


def hide_forecast_loading_overlay() -> None:
    """Remove the full-page loading overlay."""
    components.html(
        """
        <script>
        (function() {
            const doc = window.parent.document;
            const overlay = doc.getElementById("forecast-loading-overlay");
            if (overlay) overlay.remove();

            const style = doc.getElementById("forecast-loading-overlay-style");
            if (style) style.remove();
        })();
        </script>
        """,
        height=0,
        width=0,
    )

# ============================================================
# MAIN APP
# ============================================================

hero()

weeks_df = load_dataset_weeks()
shape_gdf = load_barangay_shapefile()

with st.sidebar:
    st.markdown(
        """
        <div class="sidebar-title-card">
            <div class="sidebar-title">Forecast Command Center</div>
            <div class="sidebar-subtitle">
                Choose the model mode, origin week, then launch the citywide and barangay forecast.
            </div>
            <div class="game-chip-row">
                <span class="game-chip">MODE</span>
                <span class="game-chip">WEEK</span>
                <span class="game-chip">MAP</span>
            </div>
        </div>
        """,
        unsafe_allow_html=True,
    )

    prediction_mode_label = st.radio(
        "Prediction mode",
        [
            "Standard model — uses recent dengue case data",
            "Environmental-only model — use when recent case data are unavailable",
        ],
        index=0,
    )

    mode = "standard" if prediction_mode_label.startswith("Standard") else "environmental_only"

    if weeks_df.empty:
        st.error("Could not read year/week values from data/FINAL_DATASET.xlsx.")
        selected_year = None
        selected_week = None
    else:
        available_years = sorted(weeks_df["year"].unique().tolist())

        selected_year = st.selectbox(
            "Origin year",
            available_years,
            index=len(available_years) - 1,
        )

        available_weeks = sorted(
            weeks_df.loc[weeks_df["year"] == selected_year, "week"].unique().tolist()
        )

        if selected_year == int(weeks_df["year"].min()):
            available_weeks = [w for w in available_weeks if w >= 30]

        selected_week = st.selectbox(
            "Origin week",
            available_weeks,
            index=len(available_weeks) - 1,
        )

    predict_clicked = st.button(
        "Launch Dengue Forecast",
        type="primary",
        use_container_width=True,
    )

    st.markdown(
        f"""
        <div class="status-card">
            <div class="mini-label">System status</div>
            <div class="status-line"><span>Dataset</span><span class="{'status-ok' if DATASET_PATH.exists() else 'status-bad'}">{'Found' if DATASET_PATH.exists() else 'Missing'}</span></div>
            <div class="status-line"><span>Shapefile</span><span class="{'status-ok' if SHAPE_ZIP_PATH.exists() else 'status-bad'}">{'Found' if SHAPE_ZIP_PATH.exists() else 'Missing'}</span></div>
            <div class="status-line"><span>R bridge</span><span class="{'status-ok' if R_SCRIPT_PATH.exists() else 'status-bad'}">{'Found' if R_SCRIPT_PATH.exists() else 'Missing'}</span></div>
        </div>
        """,
        unsafe_allow_html=True,
    )


col_a, col_b, col_c = st.columns(3)

with col_a:
    render_metric_card(
        "Prediction mode",
        "Standard" if mode == "standard" else "Environmental",
        "Use environmental-only when recent case records are unavailable.",
    )

with col_b:
    render_metric_card(
        "Output scope",
        "Citywide + Barangay",
        "The app shows both the city forecast and the barangay shapefile risk map.",
    )

with col_c:
    render_metric_card(
        "Forecast windows",
        "7",
        "Selected week, +1, +2, +3, +4, +8, and +12 weeks.",
    )


if not predict_clicked:
    st.info("Choose your settings in the sidebar, then click **Launch Dengue Forecast**.")
    st.stop()

if selected_year is None or selected_week is None:
    st.error("Please check that FINAL_DATASET.xlsx has valid year and week columns.")
    st.stop()


show_forecast_loading_overlay()

# Give the browser a short moment to paint the overlay before the R process starts.
time.sleep(0.25)

try:
    ensure_r_packages_ready()

    city_predictions = run_r_prediction(
        mode=mode,
        level="city",
        year=int(selected_year),
        week=int(selected_week),
    )

    barangay_predictions = run_r_prediction(
        mode=mode,
        level="barangay",
        year=int(selected_year),
        week=int(selected_week),
    )

    hide_forecast_loading_overlay()

except FileNotFoundError as e:
    hide_forecast_loading_overlay()
    st.error(str(e))
    st.info("Check that `r_scripts/predict_on_demand.R` exists inside your project folder.")
    st.stop()

except Exception as e:
    hide_forecast_loading_overlay()
    st.error("Prediction failed.")
    st.code(str(e))
    st.stop()


city_horizons = sorted(city_predictions.get("horizons", []), key=horizon_sort_key)
barangay_horizons = sorted(barangay_predictions.get("horizons", []), key=horizon_sort_key)

city_horizons = [h for h in city_horizons if int(h.get("horizon", 999)) in DISPLAY_HORIZONS]
barangay_horizons = [h for h in barangay_horizons if int(h.get("horizon", 999)) in DISPLAY_HORIZONS]

barangay_by_horizon = {int(h.get("horizon", 999)): h for h in barangay_horizons}

if not city_horizons:
    st.warning("No citywide prediction results were returned for the display horizons.")
    st.stop()

if not barangay_horizons:
    st.warning("No barangay prediction results were returned for the display horizons.")
    st.stop()


st.markdown('<div class="section-title">Forecast summary</div>', unsafe_allow_html=True)

summary_cols = st.columns(3)

with summary_cols[0]:
    st.metric("Origin week", f"{selected_year} W{selected_week}")

with summary_cols[1]:
    st.metric("Model mode", "Standard" if mode == "standard" else "Environmental-only")

with summary_cols[2]:
    st.metric("Output scope", "Citywide + Barangay")

labels = [horizon_label(h) for h in city_horizons]
tabs = st.tabs(labels)

for tab, city_h in zip(tabs, city_horizons):
    with tab:
        h_num = int(city_h.get("horizon", 999))
        barangay_h = barangay_by_horizon.get(h_num)

        label = horizon_label(city_h)
        target_year = city_h.get("target_year", "—")
        target_week = city_h.get("target_week", "—")

        st.markdown(
            f'<div class="section-title">{html.escape(label)} · {html.escape(str(target_year))} W{html.escape(str(target_week))}</div>',
            unsafe_allow_html=True,
        )

        # =====================================================
        # CITYWIDE FORECAST
        # =====================================================

        st.markdown("### Citywide dengue forecast")

        city_probability = ensure_probability(city_h)
        city_alert = city_h.get("alert_level") or city_h.get("city_status") or "Unknown"
        city_range = city_case_range(city_h)

        c1, c2, c3 = st.columns(3)

        with c1:
            st.metric("Predicted citywide case range", city_range)

        with c2:
            st.metric("Outbreak probability", fmt_percent(city_probability, 1))

        with c3:
            st.metric("Alert level/status", str(city_alert))

        city_table = pd.DataFrame(
            {
                "Forecast period": [label],
                "Target year": [target_year],
                "Target week": [target_week],
                "Predicted citywide case range": [city_range],
                "Range basis": ["Predicted cases ± MAE"],
                "Outbreak probability": [fmt_percent(city_probability, 1)],
                "Alert level/status": [str(city_alert)],
            }
        )

        render_pretty_table(city_table)

        st.markdown("#### Citywide recommended interventions")
        for item in get_intervention_plan(city_alert):
            st.markdown(f"- {item}")

        st.divider()

        # =====================================================
        # BARANGAY FORECAST
        # =====================================================

        st.markdown("### Barangay dengue risk map")

        if barangay_h is None:
            st.warning("No barangay-level prediction was returned for this forecast horizon.")
            continue

        barangay_df = prepare_barangay_rows(barangay_h)

        if barangay_df.empty:
            st.warning("No barangay-level rows were returned for this horizon.")
            continue

        total_pred = barangay_df["predicted_cases"].sum()
        mean_prob = barangay_df["probability"].dropna().mean() if "probability" in barangay_df else None

        high_count = barangay_df[
            barangay_df["alert_level"].apply(lambda x: clean_alert(x) in ["high", "very high"])
        ].shape[0]

        top_row = barangay_df.sort_values(
            ["alert_rank", "predicted_cases"],
            ascending=False,
        ).iloc[0]

        b1, b2, b3 = st.columns(3)

        with b1:
            st.metric("Total predicted cases", fmt_number(total_pred, 1))

        with b2:
            st.metric("Mean outbreak probability", fmt_percent(mean_prob, 1))

        with b3:
            st.metric("High / very high barangays", str(high_count))

        st.markdown(
            f"""
            <div class="glass-card">
                <div class="mini-label">Highest-risk barangay</div>
                <div class="big-value">{html.escape(str(top_row['barangay']))}</div>
                <div class="small-note">
                    Predicted case range: <b>{html.escape(str(top_row['predicted_case_range']))}</b><br>
                    Outbreak probability: <b>{html.escape(fmt_percent(top_row.get('probability'), 1))}</b><br>
                    Alert level:
                    <span class="alert-badge {alert_badge_class(top_row['alert_level'])}">
                        {html.escape(str(top_row['alert_level']))}
                    </span>
                </div>
            </div>
            """,
            unsafe_allow_html=True,
        )

        render_alert_legend()

        if shape_gdf is None:
            st.error("Could not load data/cebu_city_barangays.zip.")
        else:
            st.markdown('<div class="map-shell">', unsafe_allow_html=True)
            prediction_map = create_barangay_prediction_map(shape_gdf, barangay_df)
            st_folium(prediction_map, width=None, height=690, returned_objects=[])
            st.markdown("</div>", unsafe_allow_html=True)

        st.markdown("#### Barangay prediction table")

        table = barangay_df[
            [
                "barangay",
                "predicted_case_range",
                "range_basis",
                "probability_display",
                "alert_level",
                "alert_rank",
                "predicted_cases",
            ]
        ].copy()

        table = table.sort_values(["alert_rank", "predicted_cases"], ascending=False)
        table = table.drop(columns=["alert_rank", "predicted_cases"])

        table = table.rename(
            columns={
                "barangay": "Barangay",
                "predicted_case_range": "Predicted case range",
                "range_basis": "Range basis",
                "probability_display": "Outbreak probability",
                "alert_level": "Alert level",
            }
        )

        render_pretty_table(table)

        st.markdown("#### Barangay recommended interventions")
        selected_alert = str(top_row["alert_level"])
        st.caption(f"Shown for the highest current alert level in this horizon: {selected_alert}")

        for item in get_intervention_plan(selected_alert):
            st.markdown(f"- {item}")


st.markdown(
    '<div class="footer-note">Cebu City Dengue Early Warning System · Model-generated forecasts for planning support</div>',
    unsafe_allow_html=True,
)
