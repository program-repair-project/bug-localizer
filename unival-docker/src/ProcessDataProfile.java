import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class ProcessDataProfile {

    public static void main(String[] args) throws IOException {


        // Change file name to the same as the file being produced by the instrumented program
        String dataFileName = "output.txt";


        HashMap<String, HashSet<String>> causalMap = createCausalMap();
        Set<String> faultCandidates = getFaultCandidates();
        StructuredDataCollector.structureData(dataFileName, causalMap, faultCandidates);
    }

    public static HashMap<String, HashSet<String>> createCausalMap() {
        HashMap<String, HashSet<String>> causalMap = new HashMap<>();

        BufferedReader reader;
        try {
            // Change name of data file accordingly
            reader = new BufferedReader(new FileReader("CausalMap.txt"));
            String line = reader.readLine();
            while (line != null) {
                String[] row = line.split(",");
                String
                        var = row[0];
                causalMap.put(var, new HashSet<>());

                if (row.length > 1) {
                    for (int i = 1; i < row.length; i++) {
                        causalMap.get(var).add(row[i]);
                    }
                }

                line = reader.readLine();
            }
            reader.close();
            return causalMap;
        } catch (IOException e) {
            return null;
        }
    }

    public static Set<String> getFaultCandidates() {
        Set<String> faultCandidates = new HashSet<String>();

        BufferedReader reader;
        try {
            // Change name of data file accordingly
            reader = new BufferedReader(new FileReader("FaultCandidates.txt"));
            String line = reader.readLine();
            while (line != null) {

                faultCandidates.add(line);

                line = reader.readLine();
            }
            reader.close();
            return faultCandidates;
        } catch (IOException e) {
            return null;
        }
    }
}
