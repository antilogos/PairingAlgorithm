package ui;

import javax.swing.*;
import java.awt.*;

/**
 * Created by vdoquang on 17/02/16.
 */
public class MainFrame extends JFrame {

    public static JTabbedPane tabbedPane = new JTabbedPane();
    private static MainFrame instance;

    public static MainFrame getInstance() {
        if (instance == null) {
            instance = new MainFrame();
        }
        return instance;
    }

    public MainFrame() {
        super("Table Pairing Algorithm");
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

        setPreferredSize(new Dimension(900, 650));
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pack();
        setVisible(true);

        add(tabbedPane, BorderLayout.CENTER);

        tabbedPane.addTab("Log", TabDisplay.getInstance());
        tabbedPane.addTab("Pairing", TabRun.getInstance());
    }
}
