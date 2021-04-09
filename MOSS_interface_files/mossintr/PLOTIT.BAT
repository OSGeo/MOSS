echo off
mode com%2:2400,n,8,1,p
copy %1 com%2:
mode com%2:9600,n,8,1,p
