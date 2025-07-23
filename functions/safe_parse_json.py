def safe_parse_json(x):
    if not isinstance(x, str):
        return {}
    try:
        return json.loads(x)
    except json.JSONDecodeError:
        # If json.loads fails, try eval as fallback
        try:
            return eval(x)
        except:
            return {}