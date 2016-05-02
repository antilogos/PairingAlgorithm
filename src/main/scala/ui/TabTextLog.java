package ui;

import javax.swing.*;
import java.awt.*;

/**
 * Created by vdoquang on 17/02/16.
 */
public class TabTextLog extends JPanel {

    private int maxCharacter = 200000;

    private static TabTextLog instance;

    private static StringBuilder log = new StringBuilder();
    private JTextArea prompt = new JTextArea();
    private JScrollPane scrollPane = new JScrollPane(prompt);

    public static TabTextLog getInstance() {
        if (instance == null) instance = new TabTextLog();
        return instance;
    }

    public TabTextLog() {
        super();
        scrollPane.setPreferredSize(new Dimension(790,570));
        add(scrollPane);
    }

    public void log(String message) {
        log.append(message+"\n");
        if(log.length() > maxCharacter) {
            log = log.delete(0, log.length() - maxCharacter);
        }
        prompt.setText(log.toString());
        scrollPane.getVerticalScrollBar().setValue(scrollPane.getVerticalScrollBar().getMaximum());
        this.revalidate();;
    }
}
