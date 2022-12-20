# Release

Release is automated in `.github/workflows/release.yml`. The script is
triggered on tags matching `v*`. It proceeds as follows.

1. Packaging.
   - Build Python wheels for package `cpr_gym`.
2. Release.
   - Create Github release; mark as pre-release if tag contains `+`.
   - Push to PyPI; only if tag does not contain `+`.

Version numbers are derived directly from the tag, ignoring the leading
`v` where appropriate. Any valid Python version tag should work. I want
to stick to semantic versioning, i.e., use version tags like `v0.6.2`
for public releases and something like `v0.6.2+pre` for internal and
test releases.
