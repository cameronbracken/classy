#!/bin/bash

ifort -c subs.f90
ifort -c zack.f90
ifort *.o -o minos 
minos 
