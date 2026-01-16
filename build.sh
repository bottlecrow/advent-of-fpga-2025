#!/bin/bash

for day in one seven; do
  dune exec build $day > day_$day.v
done