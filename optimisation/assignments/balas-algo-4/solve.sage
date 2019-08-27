p = MixedIntegerLinearProgram(maximization=False)
v = p.new_variable(binary=True, nonnegative=True)
x1, x2, x3, x4 = v["x1"], v["x2"], v["x3"], v["x4"]
p.set_objective(-17 * x1 + -25 * x2 + -12 * x3 + -10 * x4)
p.add_constraint(3 * x1 + 6 * x2 + 5 * x3 + 5 * x4 <= 12)
p.add_constraint(4 * x2 + 9 * x2 - 2 * x3 + x4 <= 25)
p.solve()
x1opt, x2opt, x3opt, x4opt = tuple(p.get_values([x1, x2, x3, x4]))

print(x1opt, x2opt, x3opt, x4opt)

bs = [0, 1]
def objective(x1, x2, x3, x4): return 17 * x1 + 25 * x2 + 12 * x3 + 10 * x4

assign = None
obj = 0
for x1 in bs:
    for x2 in bs:
        for x3 in bs:
            for x4 in bs:
                curobj = objective(x1, x2, x3, x4)
                s = str((x1, x2, x3, x4))
                if not (3 * x1 + 6 * x2 + 5 * x3 + 5 * x4 <= 12): 
                    s += "| first infeasible"
                    print(s)
                    continue
                if not (4 * x2 + 9 * x2 - 2 * x3 + x4 <= 25): 
                    s += "| second infeasible"
                    print(s)
                    continue
                if curobj > obj: obj = curobj; assign = (x1, x2, x3, x4)
print (assign, obj)
