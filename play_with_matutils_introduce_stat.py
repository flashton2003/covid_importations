


def run_calc(Di, Ni, Do, No):
    # Di = distance to nearest in-region sample
    # Ni = number of descdents that are in region
    # Do = distance to nearest out-region sample
    # No = number of descendents that are out-region.
    print()
    print(f'Di = {Di}, Ni = {Ni}, Do = {Do}, No = {No}')
    print(1 / (1 + ((Di / Ni) / (Do / No)))) 



run_calc(1, 5, 1, 1)

run_calc(3, 5, 1, 1)

run_calc(5, 5, 1, 1)

run_calc(1, 5, 2, 3)

run_calc(0.00001, 5, 0.00001, 3)

run_calc(1, 5, 1, 3)
run_calc(0.000001, 3, 1, 2)
run_calc(0.000001, 7, 1, 14)

run_calc(0.000001, 15, 0.000001, 28)
run_calc(0.000001, 15, 0.000001, 13)

run_calc(0.000001, 3, 0.000001, 1)
run_calc(1, 2, 0.000001, 5)