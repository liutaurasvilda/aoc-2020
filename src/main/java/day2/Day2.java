package day2;

import common.ResourceReader;

import java.time.temporal.ValueRange;
import java.util.List;

public class Day2 {

    public static void main(String[] args) {
        List<String> input = ResourceReader.readAsString("day2.txt");
        System.out.println(part1(input));
        System.out.println(part2(input));
    }

    private static long part1(List<String> input) {
        return input.stream().filter(e -> {
            String[] tokens = e.split(" ");

            String[] r = tokens[0].split("-");
            ValueRange range = ValueRange.of(Integer.parseInt(r[0]), Integer.parseInt(r[1]));

            char c = tokens[1].charAt(0);
            return range.isValidValue(tokens[2].chars().filter(e1 -> e1 == c).count());
        }).count();
    }

    private static long part2(List<String> input) {
        return input.stream().filter(e -> {
            String[] tokens = e.split(" ");

            String[] r = tokens[0].split("-");
            ValueRange range = ValueRange.of(Integer.parseInt(r[0]), Integer.parseInt(r[1]));

            char c = tokens[1].charAt(0);
            return tokens[2].charAt((int) range.getMinimum() - 1) == c && !(tokens[2].charAt((int) range.getMaximum() - 1) == c) ||
                !(tokens[2].charAt((int) range.getMinimum() - 1) == c) && tokens[2].charAt((int) range.getMaximum() - 1) == c;
        }).count();
    }
}
