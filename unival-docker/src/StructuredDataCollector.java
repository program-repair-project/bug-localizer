import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

public class StructuredDataCollector {
    public static void structureData(String filePath, HashMap<String, HashSet<String>> causalMap,
            Set<String> usedVariables) {
        // 7 Columns of data processed from output file - output.txt is the default
        // Class, method, line, scope, variable, version, value
        BufferedReader reader;
        HashMap<String, ArrayList<String>> variableVersionValueArrayMap = new HashMap<>();
        try {
            BufferedWriter testWrt = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("outY.txt")));
            BufferedWriter wrtTruth = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("truth.txt")));
            BufferedWriter wrtDiff = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("diff.txt")));
            // Change name of data file accordingly
            reader = new BufferedReader(new FileReader(filePath));
            String line = reader.readLine();
            HashMap<String, String> variableVersionValueMap = new HashMap<>();

            int numExecutions = 0;
            int index = 1;
            String failIndicator = "";
            while ((line = reader.readLine()) != null) {

                if (line.contains("*** new execution ***")) {

                    failIndicator = line.split(",")[2];
                    testWrt.append(index + "\t" + failIndicator);
                    wrtTruth.write("0");
                    wrtTruth.write('\n');
                    if (failIndicator.equals("1")) {
                        wrtDiff.write("1");
                    } else {
                        wrtDiff.write("0");
                    }
                    wrtDiff.write('\n');
                    testWrt.append('\n');
                    index++;
                    testWrt.flush();
                    HashMap<String, Boolean> variableVersionValueArrayChecklist = new HashMap<>();

                    for (String variable : variableVersionValueArrayMap.keySet()) {
                        variableVersionValueArrayChecklist.put(variable, false);
                    }
                    for (String variable : variableVersionValueMap.keySet()) {
                        if (!variableVersionValueArrayMap.containsKey(variable)) {
                            variableVersionValueArrayMap.put(variable, new ArrayList<String>());
                            for (int i = 0; i < numExecutions; i++) {

                                variableVersionValueArrayMap.get(variable).add("NA");

                            }
                            variableVersionValueArrayMap.get(variable).add(variableVersionValueMap.get(variable));

                            variableVersionValueArrayChecklist.put(variable, true);
                        } else {

                            variableVersionValueArrayMap.get(variable).add(variableVersionValueMap.get(variable));
                            variableVersionValueArrayChecklist.put(variable, true);
                        }
                    }

                    for (String variable : variableVersionValueArrayChecklist.keySet()) {
                        if (!variableVersionValueArrayChecklist.get(variable)) {

                            variableVersionValueArrayMap.get(variable).add("NA");
                        }
                    }
                    variableVersionValueMap.clear();
                    line = reader.readLine();
                    numExecutions++;
                    continue;
                }
                String[] row = line.split(",");
                int len = row.length;
                // Where the magic happens with each row containing information is decomposed
                if (len == 6) {
                    // String className = row[0];
                    // String methodName = row[1];
                    // String lineNumber = row[2];
                    // String scope = row[3];
                    String variable = row[3];
                    String version = row[4];
                    Double value = 0.0;
                    String strValue = "";
                    boolean isnum = false;

                    if (row[len - 1] != null) {
                        row[len - 1] = row[len - 1].trim();
                        // if (row[len - 1].contains("@") || row[len - 1].contains("java") || row[len -
                        // 1].contains("org") || row[len - 1].contains(":") || row[len -
                        // 1].contains("[")|| row[len - 1].contains("\n")||row[len - 1].contains("\t")){
                        if (row[len - 1].contains("\n") || row[len - 1].contains("\t")) {
                            row[len - 1] = row[len - 1].replace("\n", "");
                            row[len - 1] = row[len - 1].replace("\t", "");
                        } else if (row[len - 1].equals("true")) {
                            value = 1.0;
                            isnum = true;
                        } else if (row[len - 1].equals("false")) {
                            value = 0.0;
                            isnum = true;
                        } else if (row[len - 1].equalsIgnoreCase("null")) {
                            value = Double.POSITIVE_INFINITY;
                            isnum = true;
                        } else if (row[len - 1].contains("/")) {
                            String[] fract = row[len - 1].split("/");
                            if (fract.length == 2 && isNumeric(fract[0]) && isNumeric(fract[1])) {

                                double num = Double.valueOf(fract[0]);
                                double den = Double.valueOf(fract[1]);
                                value = num / den;
                                isnum = true;
                            } else {
                                strValue = row[len - 1];
                            }
                        } else {
                            if (isNumeric(row[len - 1])) {
                                value = Double.valueOf(row[len - 1]);
                                isnum = true;
                            } else {
                                strValue = row[len - 1];
                            }
                        }
                        if (variable.startsWith("_"))
                            variable = "UNDERSCORE" + variable;

                        if (isnum) {
                            variableVersionValueMap.put(variable + "_" + version, Double.toString(value));
                        } else {
                            variableVersionValueMap.put(variable + "_" + version, strValue);
                        }

                    }
                } else {
                    continue;
                }
            }
            reader.close();

            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("newoutput.txt")));
            BufferedWriter writer2 = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream("newoutput.txt.full")));

            for (String s : variableVersionValueArrayMap.keySet()) {
                ArrayList<String> list = variableVersionValueArrayMap.get(s);

                if (Pattern.matches("^[a-zA-Z_]\\w*$", s)) {
                    writer.write(s);
                    for (int i = 0; i < list.size(); i++) {
                        try {
                            if (isNumeric(list.get(i))) {
                                writer.write("\t" + Double.parseDouble(list.get(i)));
                            } else {
                                writer.write("\t" + list.get(i));
                            }
                        } catch (NumberFormatException e) {
                            writer.write("\tNA");
                        }
                    }
                    writer.write("\n");
                    writer.flush();
                }
                writer2.write(s);
                for (int i = 0; i < list.size(); i++) {
                    try {
                        if (isNumeric(list.get(i))) {
                            writer2.write("\t" + Double.parseDouble(list.get(i)));
                        } else {
                            writer2.write("\t" + list.get(i));
                        }
                    } catch (NumberFormatException e) {
                        writer2.write("\tNA");
                    }
                }
                writer2.write("\n");
                writer2.flush();

            }

            writer.close();
            writer2.close();
            testWrt.close();
            wrtTruth.close();
            wrtDiff.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            genRForCFmeansRF("RforCFmeansRF.R", "fault_binerrs_all", "fault_binerrs", "Y", causalMap,
                    variableVersionValueArrayMap.keySet(), usedVariables);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean isDigit(String str) {
        return str.matches("-?\\d+(\\.\\d+)?"); // match a number with optional '-' and decimal.
    }

    public static boolean isNumeric(String strNum) {
        if (strNum == null) {
            return false;
        }
        try {
            double d = Double.parseDouble(strNum);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    // Method that generates the R function that will be used by the Random forest
    // (RFC.R) script
    private static void genRForCFmeansRF(String RFileName, String varFrameName, String prefix, String outName,
            HashMap<String, HashSet<String>> covariant, Set<String> usedVariables, Set<String> faultCandidates)
            throws IOException {

        OutputStream out = new FileOutputStream(RFileName);
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(out));
        BufferedWriter testInf = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("info.txt")));
        int variableCounter = 1;

        writer.write("genCFmeansRF_" + prefix + " <- function() {\n\n");
        // for RF
        writer.write("results <- list()\n\n");
        writer.write("varnames <- list()\n\n");
        // for esp
        // writer.write("results <- data.frame(row.names = \"mean\")\n\n");

        for (String t : covariant.keySet()) {
            System.out.println(t);
            if (!usedVariables.contains(t))// ||!faultCandidates.contains(t)) //Change this accordingly if restrictions
                                           // on which variables are considered exist
                continue;
            if (t.startsWith("_"))
                t = "UNDERSCORE" + t;
            // for (Value t : treatNames){
            String vfn = varFrameName;
            // for confounder
            String tfn = prefix + "_" + t + "_treat_df";
            // for no confounder
            String tfn_nocnfd = prefix + "_" + t + "_treat_nocnfd_df";

            // // for tfn
            writer.write(
                    tfn + " <- data.frame(" + outName + "=" + vfn + "$" + outName + ", " + t + "=" + vfn + "$" + t);
            HashSet<String> set = covariant.get(t);
            for (String c : set) {
                if (!usedVariables.contains(c))
                    continue;
                if (c.startsWith("_"))
                    c = "UNDERSCORE" + c;
                writer.write(", " + c + "=" + vfn + "$" + c);
            }

            // for tfn_nocnfd
            // writer.write(tfn_nocnfd + " <- data.frame(" + outName + "=" + vfn + "$" +
            // outName + ", " + t + "=" + vfn + "$" + t);

            writer.write(", stringsAsFactors = FALSE)\n");

            // to remove NA
            // writer.write(tfn + " <- " + tfn + "[complete.cases(" + tfn + "),]" + '\n');

            // Only treatement, no confounder (ESP)
            // writer.write("results[[\"" + t + "\"]] <- CFmeansForESP(" + tfn_nocnfd + ",
            // \"" + outName + "\", \"" + t + "\"");
            writer.write("numFLOut <- data.frame(" + t + "=" + "numfldata" + "$" + t);
            for (String c : set) {
                if (!usedVariables.contains(c))
                    continue;
                if (c.startsWith("_"))
                    c = "UNDERSCORE" + c;
                writer.write(", " + c + "=" + "numfldata" + "$" + c);
            }
            if (set.isEmpty()) {
                writer.write(", " + t + "=" + "numfldata" + "$" + t);
            }
            writer.write(")\n");
            // For random forest
            writer.write("id <- rownames(numFLOut)\n");
            writer.write("numFLOut<-cbind(id,numFLOut)\n");
            writer.write("write.table(numFLOut,file=\"./NUMFL/" + variableCounter + ".txt\""
                    + ",quote = F,row.names = F, col.names=T)\n");

            writer
                    .write("results[[\"" + t + "\"]] <- CFmeansForDecileBinsRF(" + tfn + ", \"" + outName + "\", \"" + t
                            + "\"");

            writer.write(")\n");
            writer
                    .write("varnames[[" + variableCounter + "]] <-  \"" + t + "\"");
            writer.write("\n\n");

            variableCounter++;
        }
        // write info for numfl and coverage based methods
        testInf.write(variableCounter + "\n");
        Random rand = new Random();
        int dummy_version = rand.nextInt(variableCounter);
        testInf.write(dummy_version + "\n");
        testInf.close();
        writer.write(
                "varframe <- data.frame(matrix(unlist(varnames), nrow = length(varnames), byrow=T),stringsAsFactors=FALSE)\n");
        writer.write("names(varframe)<- \" Variables \"\n");
        writer.write("ID<-rownames(varframe)\n");
        writer.write("varframe<-cbind(ID,varframe)\n");
        writer.write("write.csv(varframe, file=\"./NUMFL/numflvariables.csv\",row.names = F)\n\n");
        writer.write("return(results)\n\n");
        writer.write("}\n");
        writer.flush();

        writer.write("genCFmeansESP_" + prefix + " <- function() {\n\n");
        // for RF
        writer.write("results <- data.frame(row.names=seq(1, 10))\n\n");
        writer.write("Baah2010 <- data.frame(row.names=\"Baah2010\")\n\n");
        // for esp
        // writer.write("results <- data.frame(row.names = \"mean\")\n\n");

        for (String t : covariant.keySet()) {
            System.out.println(t);
            if (!usedVariables.contains(t))
                continue;
            if (t.startsWith("_"))
                t = "UNDERSCORE" + t;
            // for (Value t : treatNames){
            String vfn = varFrameName;
            // for confounder
            String tfn = prefix + "_" + t + "_treat_df";
            // for no confounder
            String tfn_nocnfd = prefix + "_" + t + "_treat_nocnfd_df";

            // // for tfn
            writer.write(
                    tfn + " <- data.frame(" + outName + "=" + vfn + "$" + outName + ", " + t + "=" + vfn + "$" + t);
            HashSet<String> set = covariant.get(t);
            for (String c : set) {
                if (!usedVariables.contains(c))
                    continue;
                if (c.startsWith("_"))
                    c = "UNDERSCORE" + c;
                writer.write(", " + c + "=" + vfn + "$" + c);
            }

            // for tfn_nocnfd
            // writer.write(tfn_nocnfd + " <- data.frame(" + outName + "=" + vfn + "$" +
            // outName + ", " + t + "=" + vfn + "$" + t);

            writer.write(", stringsAsFactors = FALSE)\n");

            // to remove NA
            // writer.write(tfn + " <- " + tfn + "[complete.cases(" + tfn + "),]" + '\n');

            // Only treatement, no confounder (ESP)
            // writer.write("results[[\"" + t + "\"]] <- CFmeansForESP(" + tfn_nocnfd + ",
            // \"" + outName + "\", \"" + t + "\"");

            // For random forest
            writer
                    .write("results[[\"" + t + "\"]] <- CFmeansForESP(" + tfn + ", \"" + outName + "\", \"" + t + "\"");
            writer.write(")\n");

            writer
                    .write("Baah2010[[\"" + t + "\"]] <- computeBaah(" + tfn + ", \"" + outName + "\", \"" + t + "\"");
            writer.write(")\n\n");
            // For LM and LASSO
            // writer.write("results[[\"" + t + "\"]] <- CFmeansForDecileBinsLM(" + tfn + ",
            // \"" + outName + "\", \"" + t + "\"");

        }
        writer.write(
                "write.csv(getTheBiggest(Baah2010),file = \"/unival/resultBaah2010.csv\")\n\n");

        writer.write("return(results)\n\n");
        writer.write("}\n");
        writer.flush();
        writer.close();
    }
}
