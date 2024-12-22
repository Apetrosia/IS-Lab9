out = open("c:/Users/Apetrosia/Desktop/progr/IS/IS-Lab9/carsss.clp", "w+")

facts = {}

out.write("\n(deffacts possible-facts")
for line in open("c:/Users/Apetrosia/Desktop/progr/IS/IS-Lab9/facts.txt", "r").readlines():
    if line.startswith('#') or line.strip() == "":
        if line.strip() == "":
            out.write("\n")
        continue
    fact_index = line.split("-")[0].strip()
    fact_name = line.split("-")[1].strip()
    facts[fact_index] = fact_name
    out.write(f"\n(possible-fact (name \"{fact_name}\") (certainty 0.0))")
out.write("\n)")

for i, line in enumerate(open("c:/Users/Apetrosia/Desktop/progr/IS/IS-Lab9/products.txt", "r").readlines()):
    if line.startswith('#') or line.strip() == "":
        if line.strip() == "":
            out.write("\n")
        continue
    from_facts = line.split("-")[0].strip().split(",")
    to_fact = line.split("-")[1].strip()
    c = line.split("-")[2].strip()

    rule_token = f"\n\n(deffacts tokens (token (name \"rule{i}\")))"
    out.write(rule_token)

    # Write second rule (if fact exists and combination is needed)
    s = f"\n\n(defrule rule{i}"
    for fact_index in from_facts:
        s += f"\n(fact (name \"{facts[fact_index.strip()]}\") (certainty ?c{fact_index.strip()}))"
        s += f"\n(test (> (abs ?c{fact_index.strip()}) 0.4))"
    s += f"\n?f <- (fact (name \"{facts[to_fact.strip()]}\") (certainty ?cf_))"
    s += f"\n?tk <- (token (name \"rule{i}\"))"
    s += "\n=>"
    s += f"\n(retract ?tk)"
    certainty = "(combine (* (min " + " ".join([f"?c{fact_index.strip()}" for fact_index in from_facts]) + f") {c}) ?cf_)"
    s += f"\n(bind ?cnew {certainty})"
    s += f"\n(modify ?f (certainty ?cnew))"
    s += f"\n(assert (sendmessage (str-cat \"{str.join(', ', [facts[ind.strip()] for ind in from_facts])} -> {facts[to_fact]}\" \" ({facts[to_fact]} \" ?cnew \")\"))))"
    out.write(s)


print("ye")
