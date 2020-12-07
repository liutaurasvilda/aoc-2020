package day4;

import common.ResourceReader;

import java.util.*;

public class Day4 {

    public static void main(String[] args) {
        List<String> input = ResourceReader.readAsString("day4.txt");
        System.out.println(part1(input));
        System.out.println(part2(input));
    }

    private static int part1(List<String> input) {
        Map<String, List<String>> passports = new HashMap<>();
        String key = UUID.randomUUID().toString();
        for (String s : input) {
            if (s.isEmpty()) {
                key = UUID.randomUUID().toString();
                continue;
            }
            passports.computeIfAbsent(key, k -> new ArrayList<>());
            passports.get(key).add(s);
        }
        Map<String, String[]> temp = new HashMap<>();
        for (Map.Entry<String, List<String>> e : passports.entrySet()) {
            temp.put(e.getKey(), String.join(" ", e.getValue()).split(" "));
        }
        int validPassports = 0;
        for (String[] v : temp.values()) {
            if (v.length == 8) {
                validPassports++;
            }
            if (v.length == 7) {
                boolean f = Arrays.stream(v).anyMatch(e -> e.contains("cid"));
                if (!f) {
                    validPassports++;
                }
            }
        }
        return validPassports;
    }

    private static int part2(List<String> input) {
        Map<String, List<String>> passports = new HashMap<>();
        String key = UUID.randomUUID().toString();
        for (String s : input) {
            if (s.isEmpty()) {
                key = UUID.randomUUID().toString();
                continue;
            }
            passports.computeIfAbsent(key, k -> new ArrayList<>());
            passports.get(key).add(s);
        }
        Map<String, String[]> temp = new HashMap<>();
        for (Map.Entry<String, List<String>> e : passports.entrySet()) {
            temp.put(e.getKey(), String.join(" ", e.getValue()).split(" "));
        }
        int validPassports = 0;
        for (String[] v : temp.values()) {

            if (v.length == 7) {
                if (Arrays.stream(v).anyMatch(e -> e.contains("cid"))) {
                    continue;
                }
            }
            if (v.length == 8 || v.length == 7) {
                boolean byr = false;
                boolean iyr = false;
                boolean eyr = false;
                boolean hgt = false;
                boolean hcl = false;
                boolean ecl = false;
                boolean pid = false;

                for (String s : v) {
                    if (s.startsWith("byr")) {
                        int b = Integer.parseInt(s.substring(4));
                        if (b >= 1920 && b <= 2002) {
                            byr = true;
                        }
                    }
                    else if (s.startsWith("iyr")) {
                        int b = Integer.parseInt(s.substring(4));
                        if (b >= 2010 && b <= 2020) {
                            iyr = true;
                        }
                    }
                    else if (s.startsWith("eyr")) {
                        int b = Integer.parseInt(s.substring(4));
                        if (b >= 2020 && b <= 2030) {
                            eyr = true;
                        }
                    }
                    else if (s.startsWith("hgt")) {
                        if (s.contains("cm")) {
                            int b = Integer.parseInt(s.substring(4, s.length()-2));
                            if (b >= 150 && b <= 193) {
                                hgt = true;
                            }
                        } else if (s.contains("in")) {
                            int b = Integer.parseInt(s.substring(4, s.length()-2));
                            if (b >= 59 && b <= 76) {
                                hgt = true;
                            }
                        }
                    }
                    else if (s.startsWith("hcl")) {
                        if (s.contains("#") && s.substring(5).length() == 6) {
                            hcl = true;
                        }
                    }
                    else if (s.startsWith("ecl")) {
                        String b = s.substring(4);
                        if (b.equals("amb") || b.equals("blu") || b.equals("brn") || b.equals("gry")
                        || b.equals("grn") || b.equals("hzl") || b.equals("oth")) {
                            ecl = true;
                        }
                    }
                    else if (s.startsWith("pid")) {
                        String b = s.substring(4);
                        if (b.length() == 9) {
                            pid = true;
                        }
                    }
                }
                if (byr && iyr && eyr && hgt && hcl && ecl && pid) {
                    validPassports++;
                }
            }
        }
        return validPassports;
    }
}
