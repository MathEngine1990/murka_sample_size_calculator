# Build a source-only archive next to the project directory, including .gitignore.
$ErrorActionPreference = 'Stop'
$releaseRoot = [System.IO.Path]::GetFullPath((Split-Path -Parent $PSScriptRoot))
$releaseArchive = Join-Path (Split-Path -Parent $releaseRoot) 'BE_Calculator_Web_GitHub.zip'
$releaseNames = @(
    '.gitignore', 'app.R', 'renv.lock', 'BE_Calculator_Web.Rproj',
    'README.md', 'DEPLOYMENT.md', 'CHANGELOG.md', 'VALIDATION.md',
    'R', 'www', 'scripts', 'tests'
)
$releaseFiles = foreach ($releaseName in $releaseNames) {
    $releasePath = Join-Path $releaseRoot $releaseName
    if (-not (Test-Path -LiteralPath $releasePath)) { throw "Missing release file: $releaseName" }
    if (Test-Path -LiteralPath $releasePath -PathType Container) {
        Get-ChildItem -LiteralPath $releasePath -Recurse -File -Force
    } else {
        Get-Item -LiteralPath $releasePath -Force
    }
}
Add-Type -AssemblyName System.IO.Compression.FileSystem
$releaseStream = [System.IO.File]::Open($releaseArchive, [System.IO.FileMode]::Create)
$releaseZip = [System.IO.Compression.ZipArchive]::new($releaseStream, [System.IO.Compression.ZipArchiveMode]::Create)
try {
    foreach ($releaseFile in $releaseFiles) {
        $releaseRelative = $releaseFile.FullName.Substring($releaseRoot.Length + 1).Replace('\', '/')
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
            $releaseZip, $releaseFile.FullName, $releaseRelative,
            [System.IO.Compression.CompressionLevel]::Optimal) | Out-Null
    }
} finally {
    $releaseZip.Dispose()
    $releaseStream.Dispose()
}
Write-Output "Created source archive: $releaseArchive ($($releaseFiles.Count) files)"
