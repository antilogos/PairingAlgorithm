package ui;

import algorithm.Manager;
import domain.Subscriber;
import util.FileOperation;

import javax.swing.*;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;
import java.util.List;

/**
 * Created by vdoquang on 17/02/16.
 */
public class TabRun extends JPanel {

    private static TabRun instance;

    private JTextArea inputField = new JTextArea();
    private JTextArea outputField = new JTextArea();
    private JButton submitButton = new JButton();

    public static TabRun getInstance() {
        if(instance == null) instance = new TabRun();
        return instance;
    }

    public TabRun() {
        super();
        inputField.setPreferredSize(new Dimension(780, 300));
        outputField.setPreferredSize(new Dimension(780, 300));
        submitButton.addActionListener(listener);
        submitButton.setText("Submit");
        add(inputField);
        add(submitButton);
        add(outputField);
    }

    private ActionListener listener = new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            if(e.getSource().equals(instance.submitButton)) {
                MainFrame.tabbedPane.setSelectedIndex(MainFrame.tabbedPane.indexOfComponent(TabDisplay.getInstance()));
                new Thread() {
                    @Override
                    public void run() {
                        String output = Manager.defaultRun(FileOperation.loadSubscriberFromText(instance.inputField.getText()));
                        outputField.setText(output);
                        instance.revalidate();
                        MainFrame.tabbedPane.setSelectedIndex(MainFrame.tabbedPane.indexOfComponent(instance));
                    }
                }.start();
            }
        }
    };
}
