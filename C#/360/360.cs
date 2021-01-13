using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections.ObjectModel;
using System.Diagnostics;

namespace temp2
{
    class Program
    {
        static Dictionary<long, long> squares = new Dictionary<long,long>();
        static Dictionary<Tuple<long, long, long>, long> triples = new Dictionary<Tuple<long, long, long>, long>();

        static void AddTriple(long a, long b, long c)
        {
            long t;
            if (a < 0)
                a = -a;
            if (b < 0)
                b = -b;
            if (c < 0)
                c = -c;
            if (a > b)
            {
                t = a; a = b; b = t;
            }
            if (a > c)
            {
                t = a; a = c; c = t;
            }
            if (b > c)
            {
                t = b; b = c; c = t;
            }
            triples[new Tuple<long, long, long>(a, b, c)] = Len(a, b, c);
        }

        static void Add(long m, long n, long p, long q, long N)
        {
            long a = m * m + n * n - p * p - q * q;
            long b = 2 * (m * q + n * p);
            long c = 2 * (n * q - m * p);
            AddTriple(N*a, N*b, N*c);
        }

        static void AddAll(long m, long n, long p, long q, long N)
        {
            Add(m, n, p, q, N); Add(m, n, p, -q, N); Add(m, -n, p, q, N); Add(m, -n, p, -q, N);
            Add(m, n, q, p, N); Add(m, n, -q, p, N); Add(m, -n, q, p, N); Add(m, -n, -q, p, N);
            Add(m, p, n, q, N); Add(m, p, n, -q, N); Add(m, p, -n, q, N); Add(m, p, -n, -q, N);
            Add(m, q, n, p, N); Add(m, -q, n, p, N); Add(m, q, -n, p, N); Add(m, -q, -n, p, N);
            Add(m, p, q, n, N); Add(m, p, -q, n, N); Add(m, p, q, -n, N); Add(m, p, -q, -n, N);
            Add(m, q, p, n, N); Add(m, -q, p, n, N); Add(m, q, p, -n, N); Add(m, -q, p, -n, N);

            Add(-m, n, p, q, N); Add(-m, n, p, -q, N); Add(-m, -n, p, q, N); Add(-m, -n, p, -q, N);
            Add(-m, n, q, p, N); Add(-m, n, -q, p, N); Add(-m, -n, q, p, N); Add(-m, -n, -q, p, N);
            Add(-m, p, n, q, N); Add(-m, p, n, -q, N); Add(-m, p, -n, q, N); Add(-m, p, -n, -q, N);
            Add(-m, q, n, p, N); Add(-m, -q, n, p, N); Add(-m, q, -n, p, N); Add(-m, -q, -n, p, N);
            Add(-m, p, q, n, N); Add(-m, p, -q, n, N); Add(-m, p, q, -n, N); Add(-m, p, -q, -n, N);
            Add(-m, q, p, n, N); Add(-m, -q, p, n, N); Add(-m, q, p, -n, N); Add(-m, -q, p, -n, N);

            Add(n, m, p, q, N); Add(n, m, p, -q, N); Add(-n, m, p, q, N); Add(-n, m, p, -q, N);
            Add(n, m, q, p, N); Add(n, m, -q, p, N); Add(-n, m, q, p, N); Add(-n, m, -q, p, N);
            Add(p, m, n, q, N); Add(p, m, n, -q, N); Add(p, m, -n, q, N); Add(p, m, -n, -q, N);
            Add(q, m, n, p, N); Add(-q, m, n, p, N); Add(q, m, -n, p, N); Add(-q, m, -n, p, N);
            Add(p, m, q, n, N); Add(p, m, -q, n, N); Add(p, m, q, -n, N); Add(p, m, -q, -n, N);
            Add(q, m, p, n, N); Add(-q, m, p, n, N); Add(q, m, p, -n, N); Add(-q, m, p, -n, N);

            Add(n, -m, p, q, N); Add(n, -m, p, -q, N); Add(-n, -m, p, q, N); Add(-n, -m, p, -q, N);
            Add(n, -m, q, p, N); Add(n, -m, -q, p, N); Add(-n, -m, q, p, N); Add(-n, -m, -q, p, N);
            Add(p, -m, n, q, N); Add(p, -m, n, -q, N); Add(p, -m, -n, q, N); Add(p, -m, -n, -q, N);
            Add(q, -m, n, p, N); Add(-q, -m, n, p, N); Add(q, -m, -n, p, N); Add(-q, -m, -n, p, N);
            Add(p, -m, q, n, N); Add(p, -m, -q, n, N); Add(p, -m, q, -n, N); Add(p, -m, -q, -n, N);
            Add(q, -m, p, n, N); Add(-q, -m, p, n, N); Add(q, -m, p, -n, N); Add(-q, -m, p, -n, N);

            Add(n, p, m, q, N); Add(n, p, m, -q, N); Add(-n, p, m, q, N); Add(-n, p, m, -q, N);
            Add(n, q, m, p, N); Add(n, -q, m, p, N); Add(-n, q, m, p, N); Add(-n, -q, m, p, N);
            Add(p, n, m, q, N); Add(p, n, m, -q, N); Add(p, -n, m, q, N); Add(p, -n, m, -q, N);
            Add(q, n, m, p, N); Add(-q, n, m, p, N); Add(q, -n, m, p, N); Add(-q, -n, m, p, N);
            Add(p, q, m, n, N); Add(p, -q, m, n, N); Add(p, q, m, -n, N); Add(p, -q, m, -n, N);
            Add(q, p, m, n, N); Add(-q, p, m, n, N); Add(q, p, m, -n, N); Add(-q, p, m, -n, N);

            Add(n, p, -m, q, N); Add(n, p, -m, -q, N); Add(-n, p, -m, q, N); Add(-n, p, -m, -q, N);
            Add(n, q, -m, p, N); Add(n, -q, -m, p, N); Add(-n, q, -m, p, N); Add(-n, -q, -m, p, N);
            Add(p, n, -m, q, N); Add(p, n, -m, -q, N); Add(p, -n, -m, q, N); Add(p, -n, -m, -q, N);
            Add(q, n, -m, p, N); Add(-q, n, -m, p, N); Add(q, -n, -m, p, N); Add(-q, -n, -m, p, N);
            Add(p, q, -m, n, N); Add(p, -q, -m, n, N); Add(p, q, -m, -n, N); Add(p, -q, -m, -n, N);
            Add(q, p, -m, n, N); Add(-q, p, -m, n, N); Add(q, p, -m, -n, N); Add(-q, p, -m, -n, N);

            Add(n, p, q, m, N); Add(n, p, -q, m, N); Add(-n, p, q, m, N); Add(-n, p, -q, m, N);
            Add(n, q, p, m, N); Add(n, -q, p, m, N); Add(-n, q, p, m, N); Add(-n, -q, p, m, N);
            Add(p, n, q, m, N); Add(p, n, -q, m, N); Add(p, -n, q, m, N); Add(p, -n, -q, m, N);
            Add(q, n, p, m, N); Add(-q, n, p, m, N); Add(q, -n, p, m, N); Add(-q, -n, p, m, N);
            Add(p, q, n, m, N); Add(p, -q, n, m, N); Add(p, q, -n, m, N); Add(p, -q, -n, m, N);
            Add(q, p, n, m, N); Add(-q, p, n, m, N); Add(q, p, -n, m, N); Add(-q, p, -n, m, N);

            Add(n, p, q, -m, N); Add(n, p, -q, -m, N); Add(-n, p, q, -m, N); Add(-n, p, -q, -m, N);
            Add(n, q, p, -m, N); Add(n, -q, p, -m, N); Add(-n, q, p, -m, N); Add(-n, -q, p, -m, N);
            Add(p, n, q, -m, N); Add(p, n, -q, -m, N); Add(p, -n, q, -m, N); Add(p, -n, -q, -m, N);
            Add(q, n, p, -m, N); Add(-q, n, p, -m, N); Add(q, -n, p, -m, N); Add(-q, -n, p, -m, N);
            Add(p, q, n, -m, N); Add(p, -q, n, -m, N); Add(p, q, -n, -m, N); Add(p, -q, -n, -m, N);
            Add(q, p, n, -m, N); Add(-q, p, n, -m, N); Add(q, p, -n, -m, N); Add(-q, p, -n, -m, N);

        }

        static long Len(long a, long b, long c)
        {
            if (a == 0)
            {
                if (b == 0)
                    return 6 * c;
                return 24 * (b + c);
            }
            return 48 * (a + b + c);            
        }

        static void Main()
        {
            for (long i = 1; i <= 25L*25L*25L*25L*25L; ++i)
                squares.Add(i * i, i);

            long N = 1;

            for (var nnn = 0; nnn < 10; ++nnn)
            {
                N *= 5;

                for (long m = 0, mm = 0; mm <= N; mm += 1 + (m << 1), ++m)
                {
                    Console.WriteLine(nnn + ":" + 100.0 * mm / N + "  " + m);
                    for (long n = m, nn = mm; nn <= N - mm; nn += 1 + (n << 1), ++n)
                    {
                        for (long q = n, qq = nn; qq <= N - mm - nn; qq += 1 + (q << 1), ++q)
                        {
                            long pp = N - mm - nn - qq;
                            if (pp < qq)
                                break;

                            if (!squares.ContainsKey(pp))
                                continue;
                            long p = squares[pp];
                            AddAll(m, n, p, q, 25L*25L*25L*25L*25L / N);
                        }
                    }
                }
            }
            long sum = 0;
            foreach (long l in triples.Values)
                sum += l;
            Console.WriteLine(sum);
            Console.ReadKey();
        }
    }
}
