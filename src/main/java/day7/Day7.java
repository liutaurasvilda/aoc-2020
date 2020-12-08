package day7;

import common.ResourceReader;

import java.util.*;

public class Day7 {

    private static final Map<String, List<String>> bagsMap = new LinkedHashMap<>();

    public static void main(String[] args) {
        List<String> input = ResourceReader.readAsString("day7.txt");
        populateMap(input);
        System.out.println(sumBagsOf(bagsMap.get("shiny gold")));
    }

    private static void populateMap(List<String> input) {
        for (String s : input) {
            String[] keyValue = s.split(" bags contain ");
            if (keyValue[1].equals("no other bags.")) {
                bagsMap.put(keyValue[0], Collections.emptyList());
            } else {
                List<String> bagsInside = new ArrayList<>();
                for (String s1 : keyValue[1].split(", ")) {
                    String[] t = s1.split(" ");
                    for (int i = 0; i < Integer.parseInt(t[0]); i++) {
                        bagsInside.add(t[1] + " " + t[2]);
                    }
                }
                bagsMap.put(keyValue[0], bagsInside);
            }
        }
    }

    private static int sumBagsOf(List<String> bags) {
        if (bags.isEmpty()) {
            return 0;
        }
        int sum = bags.size();
        for (String bag : bags) {
            sum += sumBagsOf(bagsMap.get(bag));
        }
        return sum;
    }
}

class Bag {
    private final String name;
    private final int numOfBags;

    public Bag(String name, int numOfBags) {
        this.name = name;
        this.numOfBags = numOfBags;
    }

    public String getName() {
        return name;
    }

    public int getNumOfBags() {
        return numOfBags;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Bag bag = (Bag) o;
        return Objects.equals(name, bag.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return "Bag{" +
            "name='" + name + '\'' +
            ", numOfBags=" + numOfBags +
            '}';
    }
}
