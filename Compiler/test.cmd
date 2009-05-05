@echo off
lisp.compiler && ildasm output.exe /text /item=Program && peverify output.exe && output