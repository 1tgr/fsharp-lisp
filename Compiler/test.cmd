@echo off
lisp.compiler demo.scm && ildasm output.exe /text /item=Program && peverify output.exe && output