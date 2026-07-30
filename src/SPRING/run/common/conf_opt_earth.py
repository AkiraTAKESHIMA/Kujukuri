def opt_earth(e):
    s = ''
    s += f'\
  earth_geosys: {e["geosys"]}\n'
    if e['geosys'] != 'other':
        s += f'\
  earth_rtyp: {e["rtyp"]}\n'
    else:
        s += f'\
  earth_r: {e["r"]}\n'
        for key in [key for key in ('finv', 'f', 'e2') if key in e.keys()]:
            val = e[key]
            s += f'\
  earth_{key}: {val}\n'

    return s
