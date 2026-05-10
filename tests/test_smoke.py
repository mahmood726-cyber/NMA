import shutil
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]


def _find_rscript():
    candidates = [
        shutil.which("Rscript"),
        "/mnt/c/Program Files/R/R-4.5.2/bin/Rscript.exe",
        "/mnt/c/Program Files/R/R-4.5.2/bin/x64/Rscript.exe",
        "/mnt/c/Program Files/R/R-4.5.1/bin/Rscript.exe",
        "/mnt/c/Program Files/R/R-4.5.1/bin/x64/Rscript.exe",
    ]
    for candidate in candidates:
        if candidate and Path(candidate).exists():
            return candidate
    return None


def test_core_package_files_exist():
    required = [
        ROOT / "DESCRIPTION",
        ROOT / "NAMESPACE",
        ROOT / "README.md",
        ROOT / "R" / "surroNMA.R",
        ROOT / "tests" / "testthat.R",
        ROOT / "tests" / "testthat" / "test-basic.R",
        ROOT / "e156-submission" / "index.html",
    ]
    missing = [str(path.relative_to(ROOT)) for path in required if not path.exists()]
    assert not missing, f"Missing required surroNMA files: {missing}"


def test_readme_promised_functions_exist_in_source():
    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    source = (ROOT / "R" / "surroNMA.R").read_text(encoding="utf-8")

    assert "simulate_surro_data()" in readme
    assert "surro_network(" in readme
    assert "surro_nma(" in readme
    assert "help_cmdstan_setup()" in readme
    for signature in [
        "surro_network <- function",
        "surro_nma <- function",
        "surro_nma_bayes <- function",
        "surro_nma_freq <- function",
        "simulate_surro_data <- function",
    ]:
        assert signature in source


def test_r_test_suite_is_present_and_named():
    testthat_entry = (ROOT / "tests" / "testthat.R").read_text(encoding="utf-8")
    test_file = (ROOT / "tests" / "testthat" / "test-basic.R").read_text(encoding="utf-8")

    assert 'test_check("surroNMA")' in testthat_entry
    assert "simulate_surro_data produces expected structure" in test_file
    assert "frequentist pipeline runs" in test_file


def test_r_test_suite_runs_when_rscript_is_available():
    rscript = _find_rscript()
    if not rscript:
        pytest.skip("Rscript is not installed in this environment")

    result = subprocess.run(
        [rscript, "--vanilla", "-e", "testthat::test_local('.')"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    assert result.returncode == 0, result.stderr or result.stdout
