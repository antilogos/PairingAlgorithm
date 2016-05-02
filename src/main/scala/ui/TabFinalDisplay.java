package ui;

import domain.Subscriber;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by vdoquang on 17/02/16.
 */
public class TabFinalDisplay extends JPanel {

    private static TabFinalDisplay instance;

    public static JTabbedPane tabbedPane = new JTabbedPane();
    public static List<JPanel> scrollPaneList = new ArrayList<JPanel>();
    public static List<List<JTable>> tableList = new ArrayList<List<JTable>>();
    String[] columns = {"1", "2", "", "", ""};

    public static TabFinalDisplay getInstance() {
        if (instance == null) instance = new TabFinalDisplay();
        return instance;
    }

    public TabFinalDisplay() {
        super();
        this.add(tabbedPane);
    }

    public void displayTournament(List<List<List<Subscriber>>> disposition) {
        int r = 1;
        for(List<List<Subscriber>> round : disposition) {
            JPanel roundPane = new JPanel();
            roundPane.setAutoscrolls(true);
            roundPane.setLayout(new BoxLayout(roundPane, BoxLayout.Y_AXIS));
            roundPane.setPreferredSize(new Dimension(790,560));
            for(List<Subscriber> table : round) {
                String[][] data = new String[table.size()][5];
                for(int i = 0; i < table.size(); i++) {
                    data[i][0] = table.get(i).id();
                    for(int c=0; c<table.get(i).constraints().size(); c++) {
                        data[i][1+c] = table.get(i).constraints().mkString(";").split(";")[c];
                    }
                    data[i][4] = table.get(i).group();
                }
                JTable tablePane = new JTable(data, columns);
                tablePane.setFillsViewportHeight(true);
                roundPane.add(tablePane);
                roundPane.add(new JSeparator());
            }
            tabbedPane.add("Round " + r,roundPane);
            r++;
        }
        this.revalidate();;
    }
}
