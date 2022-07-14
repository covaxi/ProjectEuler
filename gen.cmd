@echo off
for /l %%f in (1,1,803) do if not exist %%f.hs echo -- Problem %%f : http://projecteuler.net/problem=%%f > %%f.hs
