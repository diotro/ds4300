import matplotlib.pyplot as plt

def num_hash_changed(mod):
    a = [x % mod for x in range(1000)]
    b = [x % (mod + 1) for x in range(1000)]
    num_diff = 0
    for i in range(len(a)):
        if a[i] != b[i]:
            num_diff += 1
    return num_diff


mods = range(25)
hashes = [num_hash_changed(i + 1) for i in mods]

# Plot the changes
# plt.plot(mods, hashes)
# plt.show()

hash_changes = []
for index in range(len(hashes) - 1):
    hash_changes.append(hashes[index+1] - hashes[index])


plt.plot(mods, hashes)
plt.plot(range(24), hash_changes)
plt.show()

