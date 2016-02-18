package ui;

import javax.swing.*;
import java.awt.*;

/**
 * Created by vdoquang on 17/02/16.
 */
public class TabDisplay extends JPanel {

    private int maxCharacter = 200000;

    private static TabDisplay instance;

    private static StringBuilder log = new StringBuilder();
    private JTextArea prompt = new JTextArea();
    private JScrollPane scrollPane = new JScrollPane(prompt);

    public static TabDisplay getInstance() {
        if (instance == null) instance = new TabDisplay();
        return instance;
    }

    public TabDisplay() {
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
        this.revalidate();;
        scrollPane.getHorizontalScrollBar().setValue(scrollPane.getVerticalScrollBar().getMaximum());
    }
}
