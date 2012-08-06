using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SuffixArray
{
    public class LCP
    {
        public static int[] sufsort(string T)
        {
            int n = T.Length;
            int[] SA = new int[n];
            if (SuffixArray.SAIS.sufsort(T, SA, n) == 0)
                return SA;
            return null;
        }
        public static int[] computeLCP(string input, int[] sa)
        {
            return computeLCP(input, 0, input.Length, sa);
        }
        public static int [] computeLCP(string input, int start, int length, int [] sa)
        {
            int [] rank = new int [length];
            for (int i = 0; i < length; i++)
                rank[sa[i]] = i;
            int h = 0;
            int [] lcp = new int [length];
            for (int i = 0; i < length; i++)
            {
                int k = rank[i];
                if (k == 0)
                {
                    lcp[k] = -1;
                }
                else
                {
                    int j = sa[k - 1];
                    while (i + h < length && j + h < length
                        && input[start + i + h] == input[start + j + h])
                    {
                        h++;
                    }
                    lcp[k] = h;
                }
                if (h > 0) h--;
            }
            return lcp;
        }
    }
}
