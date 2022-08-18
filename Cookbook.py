def test(recipe):
    return ["pytest test/test_lispy-python.py"]

def typecheck(recipe):
    return "dmypy run lispy-python.py"

del typecheck
