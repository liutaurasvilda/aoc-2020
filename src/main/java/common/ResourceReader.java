package common;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public final class ResourceReader {

    private ResourceReader() {
    }

    public static List<String> readAsString(String resourcePath) {
        try {
            return Files.readAllLines(
                Paths.get(ClassLoader.getSystemResource(resourcePath).toURI()));
        } catch (IOException | URISyntaxException e) {
            return Collections.emptyList();
        }
    }

    public static List<Integer> readAsInt(String resourcePath) {
        return readAsString(resourcePath)
            .stream().map(Integer::valueOf).collect(Collectors.toList());
    }

    public static List<Long> readAsLong(String resourcePath) {
        return readAsString(resourcePath)
            .stream().map(Long::valueOf).collect(Collectors.toList());
    }
}
