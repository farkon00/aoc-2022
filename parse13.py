with open("input/input13.txt") as f:
    content = f.read()

pairs_text = content.split("\n\n")
pairs_separated = [i.split("\n") for i in pairs_text]
pairs = [(eval(i[0]), eval(i[1])) for i in pairs_separated]

def generate_data_text(data):
    if isinstance(data, int):
        return f"Data (Left {data})"
    else:
        result = "Data (Right ["
        for i in data:
            result += generate_data_text(i) + ","
        result = result.removesuffix(",")
        result += "])"
        return result

result = "["
for i in pairs:
    result += "("
    result += generate_data_text(i[0])
    result += ","
    result += generate_data_text(i[1])
    result += "),"
result = result[:-1]
result += "]"

print(result)