import time

def str_to_bits(s):
    return [int(c) for c in s]


def bits_to_str(bits):
    return ''.join(str(b) for b in bits)


def trivium_init(key, iv):
    # Key and IV are loaded into the register in LSB first order
    key = key[::-1]
    iv = iv[::-1]

    NLFSR1 = key + [0] * (93 - len(key))
    NLFSR2 = iv + [0] * (84 - len(iv))
    NLFSR3 = [0] * 108 + [1, 1, 1]  # Last three bits should be 1

    return NLFSR1, NLFSR2, NLFSR3


def trivium_clock(A, B, C):
    # Perform one Trivium clock cycle and return the keystream bit
    t1 = A[65] ^ (A[90] & A[91]) ^ A[92] ^ B[77]
    t2 = B[68] ^ (B[81] & B[82]) ^ B[83] ^ C[86]
    t3 = C[65] ^ (C[108] & C[109]) ^ C[110] ^ A[68]

    z = A[65] ^ A[92] ^ B[68] ^ B[83]^C[65]^C[110]# Keystream bit is produced before updating registers

    # Update registers
    A = [t3] + A[:-1]
    B = [t1] + B[:-1]
    C = [t2] + C[:-1]

    return A, B, C, z


def trivium_encrypt(key_str, iv_str, plaintext_str):
    key = str_to_bits(key_str)
    iv = str_to_bits(iv_str)
    plaintext = str_to_bits(plaintext_str)

    A, B, C = trivium_init(key, iv)

    # print("Initial state:")
    # print("NLFSR1(93 bits): ", bits_to_str(A))
    # print("NLFSR2(84 bits): ", bits_to_str(B))
    # print("NLFSR3(111 bits):", bits_to_str(C))
    # print("-" * 80)

    # Warm-up phase (1152 cycles)
    for _ in range(1152):
        A, B, C, _ = trivium_clock(A, B, C)

    # print("After warm up(1152 updates):")
    # print("NLFSR1(93 bits): ", bits_to_str(A))
    # print("NLFSR2(84 bits): ", bits_to_str(B))
    # print("NLFSR3(111 bits):", bits_to_str(C))
    # print("-" * 80)


# Generate keystream
    keystream = []
    for _ in range(len(plaintext)):
        A, B, C, z = trivium_clock(A, B, C)
        keystream.append(z)

    # print("Key stream: ", bits_to_str(keystream))
    # print("-" * 80)

    # print("After key stream generation:")
    # print("NLFSR1(93 bits): ", bits_to_str(A))
    # print("NLFSR2(84 bits): ", bits_to_str(B))
    # print("NLFSR3(111 bits):", bits_to_str(C))
    # print("-" * 80)


    ciphertext = [p ^ k for p, k in zip(plaintext, keystream)]

    # print("Ciphertext: ", bits_to_str(ciphertext))
    return bits_to_str(ciphertext)


# === 输入数据 ===
key = "00000001000000100000001100000100000001010000011000000111000010000000100100001010"
iv = "00000000000000000000000000000000000000000000000000000000000000000000000000000000"
plaintext = "00000000000000000000000000000000000000000000000000000000000000000000000000000000"
expected = "11001110101011110100000011011101011100001001001000111011001011000001000011000100"

# === 执行 ===
start_time = time.time()
output = trivium_encrypt(key, iv, plaintext)
end_time = time.time()

print("Output:", output)
print("Execution time:", end_time - start_time, "seconds")

# print("Output matches expected:", output == expected)