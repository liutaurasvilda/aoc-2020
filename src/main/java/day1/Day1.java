package day1;

import common.ResourceReader;

import java.util.List;

public final class Day1 {

    public static void main(String[] args) {
        List<Integer> input = ResourceReader.readAsInt("day1.txt");
        System.out.println(part1(input));
        System.out.println(part2(input));
    }

    private static int part1(List<Integer> input) {
        for (int i = 0; i < input.size(); i++) {
            for (int j = i + 1; j < input.size(); j++) {
                if (input.get(i) + input.get(j) == 2020) {
                    return input.get(i) * input.get(j);
                }
            }
        }
        return -1;
    }

    private static int part2(List<Integer> input) {
        for (int i = 0; i < input.size(); i++) {
            for (int j = i + 1; j < input.size(); j++) {
                for (int k = j + 1; k < input.size(); k++)
                    if (input.get(i) + input.get(j) + input.get(k) == 2020) {
                        return input.get(i) * input.get(j) * input.get(k);
                    }
            }
        }
        return -1;
    }
}
