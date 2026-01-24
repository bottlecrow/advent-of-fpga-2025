#!/bin/bash

for day in one seven eleven; do
  dune exec build $day > day_$day.v
done