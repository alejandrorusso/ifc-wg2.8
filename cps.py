#!/usr/bin/python

import ast

def D(s):
    print type(s)
    def D2(gamma):
            if type(s)==ast.Module and not len(s.body): #epsilon-rule the sequence is empty. It is like skip
		 return lambda sigma : [sigma]+gamma(sigma) #[sigma, gamma(sigma)...]
            elif type(s)==ast.Module and len(s.body): #Seq-rule
                 s1=s.body.pop(0)
                 return D(s1)(D(s)(gamma))
            elif type(s)==ast.Assign: #Assign-rule
		 def a(sigma):
		      # target=[s1.targets[0].id]
		      # value=s1.value.n
                      sigma.update([(t.id,s.value.n) for t in s.targets])
		      return [sigma].extend(gamma(sigma)) #[sigma, gamma(sigma)...]
                 return a
            else: return "not supported"
    return D2

def run(source):
    code=ast.parse(source)
    empty_sigma={}
    r=D(code)(lambda x : [x])(empty_sigma)
    return r

run('x=1;y=2')
