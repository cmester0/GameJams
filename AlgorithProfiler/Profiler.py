import tokenize
import sys
import ast

generator = tokenize.generate_tokens(sys.stdin.readline)

tree = ast.parse(sys.stdin.read())

function_complexity = {
    "map":   lambda x: ["[v0][v1]"] * len(x),
    "int":   lambda _: ["1"],
    "input": lambda x: ["[io]"], # [n]
    "readlines": lambda x: ["[io]"], # [n]
    "range": lambda x: ["[v0]"], # [n]
    "split": lambda x: ["[v0]"], # ??
    "print": lambda _: ["[v0]"],
    "set":   lambda _: ["1"], # for constructor, if function call then [v0]
    "min":   lambda _: ["[min]"],
    "reversed":   lambda _: ["[v0]"],
    "join":   lambda _: ["[v0]"],
    "list":   lambda _: ["[v0]"],
    "append":   lambda _: ["1"],
    "add":   lambda _: ["1"],
    "remove":   lambda _: ["1"],
    "len":   lambda _: ["1"],
    "insert":   lambda _: ["[v0]"],
}

def simplify_complexity_expression(c):
    c = c.replace("1","")
    c = "+".join(list(set(c.split("+"))))
    c = c.replace("++","+")
    if len(c) > 0:
        if c[0] == "+":
            c = c[1:]
        if c[-1] == "+":
            c = c[:-2]
    return c

# def ast_assign():

def analyze_value(val, targets = [""]):
    # print (type(val))
    if type(val) == ast.Name:
        if val.id in function_complexity:
            return [(val.id, function_complexity[val.id](targets))]

        return [("", [val.id])]
    # TODO:
    elif type(val) == ast.Attribute:
        c = analyze_value(val.value)
        
        print (function_complexity[val.attr](["0"] * len(c)))
        return [(val.attr, function_complexity[val.attr]((["0"] * len(c))))]
    elif type(val) == ast.ImportFrom or type(val) == ast.Import:
        return [("",["1"])]    
    # TODO: Not constant (eg. in)
    elif type(val) == ast.Compare:
        return [("",["1"])]
    elif type(val) == ast.UnaryOp:
        return [("",["1"])]
    elif type(val) == ast.Subscript:
        return [("",["1"])]
    elif type(val) == ast.Constant or type(val) == ast.Dict:
        return [("",["1"])]
    elif type(val) == ast.Tuple or type(val) == ast.List:
        if len(val.elts) == 0:
            return [("", ["1"])]
        return list(map (lambda x: x[0], map (analyze_value, val.elts)))
    elif type(val) == ast.Call:
        fun = analyze_value(val.func, targets)

        if len(fun) != 1:
            print ("len not 1", fun)
            return

        fname, fcompfun = fun[0]
        fcompfunargs = []
        for vl in map(lambda x: analyze_value(x[1],
                                         targets if x[0] >= len(targets) else targets[x[0]]),
                         list(enumerate(val.args))):
            fcompfunargs.append(["+".join(vb) for va, vb in vl])

        for i, v in enumerate(fcompfunargs):
            fcompfun = [l.replace("[v" + str(i) + "]", v[0]) for l in fcompfun]
            
        return [(fname, fcompfun)] # (lambda x: fcompres (fcompfun x))
    elif type(val) == ast.ListComp:
        listcomp = analyze_value(val.elt)

        if len(listcomp) != 1:
            print ("len not 1", listcomp)
            return

        result = []
        for comp in val.generators:
            rangecomp = analyze_value(comp)

            if len(rangecomp) != 1:
                print ("len not 1", rangecomp)
                return

            result.append(rangecomp[0][1])
        
        return [("",["*".join(k) for k in result])]

    elif type(val) == ast.comprehension:
        # print (ast.dump(val))
        return analyze_value(val.iter)

    # TODO:
    elif type(val) == ast.If:
        complexity = analyze_value(val.test)
        return complexity

    elif type(val) == ast.Return:
        return analyze_value(val.value)

    # TODO: add together
    elif type(val) == ast.BinOp:
        analyze_value(val.left)
        return analyze_value(val.right)

    # TODO: Do not give up on whiles ! (invariants?)
    elif type(val) == ast.While:
        return [("",["[inf]"])]

    # TODO: Lambda
    elif type(val) == ast.Lambda:
        return [("",["[lambda]"])]

    # TODO: BoolOp
    elif type(val) == ast.BoolOp:
        return [("",["[BoolOp]"])]


    # TODO:        
    elif type(val) == ast.AugAssign:
        # target = list(map(lambda x: x[1], analyze_value(val.target)))
        # print ("tAr", target)
        complexity = analyze_value(val.value) # target
        return complexity
        
        # _, comp = complexity[0]
                
        # result = []
        # for t, c in zip(targets, comp):
        #     complexity_val = [t if simplify_complexity_expression(c) == "[io]" else c]
        #     temp = function_complexity[t]
        #     function_complexity[t] = lambda x: temp(x) + complexity_val
        #     result.append((t, complexity_val))
    
        # return result
        
    elif type(val) == ast.Assign:
        # print ("vals", val.targets)
    
        targets = list(map(analyze_value, val.targets))[0]
        targets = list(map(lambda x: x[1][0], targets))
        complexity = analyze_value(val.value, targets)
                    
        _, comp = complexity[0]
                
        result = []
        for t, c in zip(targets, comp):
            complexity_val = [t if simplify_complexity_expression(c) == "[io]" else c]
            function_complexity[t] = lambda x: complexity_val
            result.append((t, complexity_val))
            
        return result
            
    elif type(val) == ast.FunctionDef:
        print ()
        print ("****",val.name,"START","****")
        def_complexity = []    
        for elem in val.body:
            print (elem)
            complexity = analyze_value(elem)
                
            for _, c in complexity:
                print (c)
                def_complexity.append("+".join(map(simplify_complexity_expression,c)))

        complexity_val = [simplify_complexity_expression("+".join(def_complexity))]
        function_complexity[val.name] = lambda x: complexity_val
        
        print ("****",val.name,complexity_val,"END","****")
        print ()

        return [(val.name, complexity_val)]

    elif type(val) == ast.For:
        print ()
        print ("*** for  ***")
        def_complexity = []    
        for elem in val.body:
            complexity = analyze_value(elem)

            for _, c in complexity:
                def_complexity.append("+".join(map(simplify_complexity_expression,c)))

        complexity_val = [simplify_complexity_expression("+".join(def_complexity))]
        print (complexity_val)
        print ("*** end  ***")

        return [("", complexity_val)]        
        
    elif type(val) == ast.Expr:
        return analyze_value(val.value)
        
    else:
        print ("missing: " + ast.dump(val))
        return

for t in tree.body:
    print (analyze_value(t))

print ("\nComplexity")
for a in function_complexity:
    print (a, function_complexity[a](["x"]))
