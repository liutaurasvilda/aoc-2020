package day9;

import common.ResourceReader;

import java.util.Collections;
import java.util.List;

public class Day9 {

    public static void main(String[] args) {
        List<Long> input = ResourceReader.readAsLong("day9.txt");
        long part1 = part1(input, 25);
        System.out.println(part1);
        System.out.println(part2(input, part1));
    }

    private static long part1(List<Long> input, int preamble) {
        for (int i = preamble; i < input.size(); i++) {
            int start = i - preamble;
            int end = i - 1;
            boolean found = false;
            for (int j = start; j < end; j++) {
                for (int k = j+1; k < i; k++) {
                    long c = input.get(i);
                    long a = input.get(j);
                    long b = input.get(k);
                    if (a != b && (a + b) == c) {
                        found = true;
                        break;
                    }
                }
            }
            if (!found) {
                return input.get(i);
            }
        }
        return -1;
    }

    private static long part2(List<Long> input, long n) {
        input.remove(n);
        for (int i = 0; i < input.size()-1; i++) {
            for (int j = i + 1; j < input.size(); j++) {
                List<Long> temp = input.subList(i, j+1);
                long sum = temp.stream().reduce((long)0, Long::sum);
                if (sum == n) {
                    Collections.sort(temp);
                    return temp.get(0) + temp.get(temp.size()-1);
                }
            }
        }
        return -1;
    }
}
