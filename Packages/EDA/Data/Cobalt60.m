BeginPackage["EDA`Data`Cobalt60`"]

Cobalt60Data::usage = 
	"Cobalt60Data is part of a cobalt-60 nuclear spectrum. The
	format is: {channel, counts}. The data was collected in the second-year
	Lab of the Department of Physics, University of Toronto with a NaI 
	scintillator and a PC-based multichannel analyzer running PC-MCA software 
	by David Harrison (1990, unpublished)."

(*
 * Part of a Cobalt-60 spectrum.  The data pairs are {channel, counts}.
 * The data was taken in the II Year Lab of the Dept. of Physics, Univ.
 * of Toronto with a NaI scintillator and a PC-based multichannel analyzer
 * running PC-MCA software by David Harrison in November, 1990.
 *)
Cobalt60Data = {
{ 1700, 45 }, { 1701, 47 }, { 1702, 42 }, { 1703, 41 }, { 1704, 31 },
{ 1705, 36 }, { 1706, 43 }, { 1707, 43 }, { 1708, 44 }, { 1709, 48 },
{ 1710, 36 }, { 1711, 42 }, { 1712, 42 }, { 1713, 39 }, { 1714, 28 },
{ 1715, 39 }, { 1716, 36 }, { 1717, 42 }, { 1718, 31 }, { 1719, 31 },
{ 1720, 38 }, { 1721, 39 }, { 1722, 42 }, { 1723, 45 }, { 1724, 45 },
{ 1725, 57 }, { 1726, 59 }, { 1727, 35 }, { 1728, 40 }, { 1729, 49 },
{ 1730, 56 }, { 1731, 37 }, { 1732, 49 }, { 1733, 55 }, { 1734, 41 },
{ 1735, 49 }, { 1736, 47 }, { 1737, 53 }, { 1738, 42 }, { 1739, 60 },
{ 1740, 56 }, { 1741, 63 }, { 1742, 51 }, { 1743, 63 }, { 1744, 50 },
{ 1745, 47 }, { 1746, 61 }, { 1747, 37 }, { 1748, 58 }, { 1749, 64 },
{ 1750, 56 }, { 1751, 59 }, { 1752, 62 }, { 1753, 66 }, { 1754, 71 },
{ 1755, 70 }, { 1756, 69 }, { 1757, 70 }, { 1758, 67 }, { 1759, 67 },
{ 1760, 69 }, { 1761, 57 }, { 1762, 75 }, { 1763, 75 }, { 1764, 80 },
{ 1765, 71 }, { 1766, 83 }, { 1767, 87 }, { 1768, 74 }, { 1769, 80 },
{ 1770, 87 }, { 1771, 81 }, { 1772, 84 }, { 1773, 99 }, { 1774, 94 },
{ 1775, 112 }, { 1776, 108 }, { 1777, 98 }, { 1778, 101 }, { 1779, 90 },
{ 1780, 94 }, { 1781, 102 }, { 1782, 107 }, { 1783, 98 }, { 1784, 104 },
{ 1785, 115 }, { 1786, 113 }, { 1787, 114 }, { 1788, 124 }, { 1789, 138 },
{ 1790, 139 }, { 1791, 132 }, { 1792, 102 }, { 1793, 143 }, { 1794, 138 },
{ 1795, 131 }, { 1796, 131 }, { 1797, 158 }, { 1798, 126 }, { 1799, 152 },
{ 1800, 147 }, { 1801, 159 }, { 1802, 158 }, { 1803, 146 }, { 1804, 148 },
{ 1805, 143 }, { 1806, 151 }, { 1807, 149 }, { 1808, 171 }, { 1809, 143 },
{ 1810, 152 }, { 1811, 170 }, { 1812, 155 }, { 1813, 156 }, { 1814, 173 },
{ 1815, 169 }, { 1816, 168 }, { 1817, 183 }, { 1818, 160 }, { 1819, 175 },
{ 1820, 170 }, { 1821, 180 }, { 1822, 199 }, { 1823, 142 }, { 1824, 171 },
{ 1825, 198 }, { 1826, 184 }, { 1827, 162 }, { 1828, 178 }, { 1829, 170 },
{ 1830, 179 }, { 1831, 178 }, { 1832, 211 }, { 1833, 149 }, { 1834, 187 },
{ 1835, 176 }, { 1836, 213 }, { 1837, 168 }, { 1838, 181 }, { 1839, 169 },
{ 1840, 191 }, { 1841, 182 }, { 1842, 176 }, { 1843, 166 }, { 1844, 185 },
{ 1845, 162 }, { 1846, 171 }, { 1847, 177 }, { 1848, 149 }, { 1849, 182 },
{ 1850, 190 }, { 1851, 137 }, { 1852, 172 }, { 1853, 168 }, { 1854, 174 },
{ 1855, 157 }, { 1856, 160 }, { 1857, 159 }, { 1858, 140 }, { 1859, 160 },
{ 1860, 158 }, { 1861, 143 }, { 1862, 143 }, { 1863, 152 }, { 1864, 152 },
{ 1865, 128 }, { 1866, 142 }, { 1867, 160 }, { 1868, 125 }, { 1869, 138 },
{ 1870, 152 }, { 1871, 121 }, { 1872, 117 }, { 1873, 124 }, { 1874, 107 },
{ 1875, 112 }, { 1876, 120 }, { 1877, 112 }, { 1878, 121 }, { 1879, 111 },
{ 1880, 131 }, { 1881, 116 }, { 1882, 116 }, { 1883, 100 }, { 1884, 101 },
{ 1885, 99 }, { 1886, 93 }, { 1887, 86 }, { 1888, 88 }, { 1889, 103 },
{ 1890, 81 }, { 1891, 101 }, { 1892, 74 }, { 1893, 79 }, { 1894, 89 },
{ 1895, 77 }, { 1896, 62 }, { 1897, 61 }, { 1898, 70 }, { 1899, 77 },
{ 1900, 68 }, { 1901, 64 }, { 1902, 67 }, { 1903, 63 }, { 1904, 75 },
{ 1905, 54 }, { 1906, 54 }, { 1907, 50 }, { 1908, 57 }, { 1909, 48 },
{ 1910, 47 }, { 1911, 60 }, { 1912, 50 }, { 1913, 43 }, { 1914, 51 },
{ 1915, 50 }, { 1916, 35 }, { 1917, 30 }, { 1918, 41 }, { 1919, 42 },
{ 1920, 42 }, { 1921, 30 }, { 1922, 47 }, { 1923, 30 }, { 1924, 33 },
{ 1925, 36 }, { 1926, 33 }, { 1927, 23 }, { 1928, 41 }, { 1929, 32 },
{ 1930, 35 }, { 1931, 26 }, { 1932, 26 }, { 1933, 18 }, { 1934, 20 },
{ 1935, 26 }, { 1936, 14 }, { 1937, 23 }, { 1938, 23 }, { 1939, 20 },
{ 1940, 19 }, { 1941, 22 }, { 1942, 21 }, { 1943, 11 }, { 1944, 24 },
{ 1945, 24 }, { 1946, 16 }, { 1947, 17 }, { 1948, 14 }, { 1949, 18 },
{ 1950, 16 }, { 1951, 14 }, { 1952, 17 }, { 1953, 13 }, { 1954, 17 },
{ 1955, 20 }, { 1956, 21 }, { 1957, 15 }, { 1958, 17 }, { 1959, 17 },
{ 1960, 12 }, { 1961, 13 }, { 1962, 13 }, { 1963, 20 }, { 1964, 13 },
{ 1965, 12 }, { 1966, 13 }, { 1967, 13 }, { 1968, 20 }, { 1969, 26 },
{ 1970, 21 }, { 1971, 15 }, { 1972, 13 }, { 1973, 17 }, { 1974, 16 },
{ 1975, 18 }, { 1976, 20 }, { 1977, 19 }, { 1978, 12 }, { 1979, 14 },
{ 1980, 29 }, { 1981, 19 }, { 1982, 19 }, { 1983, 18 }, { 1984, 18 },
{ 1985, 16 } };

EndPackage[]