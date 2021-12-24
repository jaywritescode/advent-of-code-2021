from collections import namedtuple
from more_itertools import take
from more_itertools.more import chunked, peekable


Packet = namedtuple('Packet', ['version', 'type_id', 'payload'])


def hex2bin(bits: str) -> str:
    return ''.join("{:04b}".format(int(c, base=16)) for c in bits)

def bits2dec(iterable):
    return int(''.join(iterable), base=2)


def to_packets(hex):
    return parse_next_packet(iter(hex2bin(hex)))

def resolve(packet):
    type_id = packet.type_id
    if type_id == 4:
        return packet.payload
    elif type_id == 0:
        return sum(resolve(p) for p in packet.payload)
    elif type_id == 1:
        i = 1
        for p in packet.payload:
            i *= resolve(p)
        return i
    elif type_id == 2:
        return min(resolve(p) for p in packet.payload)
    elif type_id == 3:
        return max(resolve(p) for p in packet.payload)
    elif type_id == 5:
        return 1 if resolve(packet.payload[0]) > resolve(packet.payload[1]) else 0
    elif type_id == 6:
        return 1 if resolve(packet.payload[0]) < resolve(packet.payload[1]) else 0
    elif type_id == 7:
        return 1 if resolve(packet.payload[0]) == resolve(packet.payload[1]) else 0
    else:
        raise

def parse_next_packet(bitstream):
    version = bits2dec(take(3, bitstream))
    type_id = bits2dec(take(3, bitstream))

    if type_id == 4:
        payload = parse_literal_value(bitstream)
    else:
        payload = parse_operator_packet(bitstream)

    return Packet(version, type_id, payload)

def parse_literal_value(bitstream):
    binary_value = []
    chunks = chunked(bitstream, 5)
    while True:
        chunk = next(chunks)
        is_last, payload = chunk[0], chunk[1:]
        binary_value.extend(payload)
        if is_last == '0':
            return bits2dec(binary_value)

def parse_operator_packet(bitstream):
    if next(bitstream) == '0':
        return parse_subpackets_given_length(bitstream)
    else:
        return parse_subpackets_given_count(bitstream)

def parse_subpackets_given_length(bitstream):
    length = bits2dec(take(15, bitstream))
    p = peekable(take(length, bitstream))
    
    subpackets = []
    while p.peek(None):
        subpackets.append(parse_next_packet(p))
    return subpackets
        
def parse_subpackets_given_count(bitstream):
    count = bits2dec(take(11, bitstream))
    
    subpackets = []
    for _ in range(count):
        subpackets.append(parse_next_packet(bitstream))
    return subpackets

def version_sum(root_packet):
    n = root_packet.version
    s = 0 if root_packet.type_id == 4 else sum(version_sum(p) for p in root_packet.payload)

    return n + s
    
def solve(puzzle):
    packet = to_packets(puzzle)
    return resolve(packet)


if __name__ == '__main__':
    with open('input-16.txt') as file:
        print(solve(file.readline().strip()))