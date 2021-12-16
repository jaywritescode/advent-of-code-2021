#%%
from more_itertools import take
from more_itertools.more import chunked

def hex2bin(bits):
    return bin(int(bits, base=16))[2:]

def bits2dec(iterable):
    print(f"bits2dec: {iterable}")
    return int(''.join(iterable), base=2)

def parse_next_packet(bitstream):
    i = iter(bitstream)
    version = bits2dec(take(3, i))
    type_id = bits2dec(take(3, i))

    if type_id == 4:
        payload = parse_literal_value(i)
    else:
        payload = parse_operator_packet(i)

    print(version)
    print(type_id)
    print(payload)
#%%
def parse_literal_value(bitstream):
    binary_value = []
    chunks = chunked(bitstream, 5)
    while True:
        chunk = next(chunks)
        print(f"chunk: {chunk}")
        is_last, payload = chunk[0], chunk[1:]
        binary_value.extend(payload)
        if is_last == '0':
            return bits2dec(binary_value)



# %%

def parse_operator_packet(bitstream):
    if next(bitstream) == '0':
        return parse_subpackets_given_length(bitstream)
    else:
        return parse_subpackets_given_count(bitstream)

def parse_subpackets_given_length(bitstream):
    length = bits2dec(take(15, bitstream))
    subbitstream = take(length, bitstream)
    
    subpackets = []
    while subbitstream:
        subpackets.append(parse_next_packet(subbitstream))
    return subpackets

def parse_subpackets_given_count(bitstream):
    count = bits2dec(take(11, bitstream))
    subpackets = []
    for _ in range(count):
        subpackets.append(parse_next_packet(bitstream))
    return subpackets