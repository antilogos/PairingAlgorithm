package ui;

import algorithm.Manager;
import domain.Subscriber;
import util.FileOperation;

import javax.swing.*;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
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
                MainFrame.tabbedPane.setSelectedIndex(MainFrame.tabbedPane.indexOfComponent(TabTextLog.getInstance()));
                new Thread() {
                    @Override
                    public void run() {
                        scala.collection.immutable.List<scala.collection.immutable.List<scala.collection.immutable.List<Subscriber>>> roundDisposition
                                = Manager.defaultRun(FileOperation.loadSubscriberFromText(instance.inputField.getText()));
                        //outputField.setText(output);
                        MainFrame.tabbedPane.add("Disposition",TabFinalDisplay.getInstance());
                        List<List<List<Subscriber>>> javaList = new ArrayList<List<List<Subscriber>>>();
                        for(scala.collection.immutable.List<scala.collection.immutable.List<Subscriber>> round : scala.collection.JavaConversions.asJavaList(roundDisposition)) {
                            List<List<Subscriber>> javaRound = new ArrayList<List<Subscriber>>();
                            for(scala.collection.immutable.List<Subscriber> table : scala.collection.JavaConversions.asJavaList(round)) {
                                List<Subscriber> javaTable = scala.collection.JavaConversions.asJavaList(table);
                                javaRound.add(javaTable);
                            }
                            javaList.add(javaRound);
                        }
                        TabFinalDisplay.getInstance().displayTournament(javaList);
                        MainFrame.tabbedPane.setSelectedIndex(MainFrame.tabbedPane.indexOfComponent(TabFinalDisplay.getInstance()));
                    }
                }.start();
            }
        }
    };
}
