# ALZW (Arithmetic LZW)
A compression scheme combining arithmetic coding and LZW - Created as a class project for Topics in Advanced Multimedia Technologies (80240553) by Run Xuan Yang

This program is implemented in F# 4.1 (FSharp.Core 4.4.1.0) using Microsoft Visual Studio 2017, and requires .NET Framework 4.5 to run.

```
Usage:
    Alzw.exe (enc|dec) inputFile outputFile [-b blockSize] [-e forbiddenRegion]

blockSize: Number of bytes processed before each forget-routine. Must be an
           integer between [1024, 16777216]. Default is 32768.

forbiddenRegion: Size of the forbidden region over 10000. Must be an integer
                 between [1, 9998] (need 1 for EOF and at least 1 for the
                 alphabets). Default is 30.
```
