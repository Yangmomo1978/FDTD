import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class Gui extends JFrame{
  
  private JTextField SizeXfield; 
  private JTextField SizeYfield; 
  static public JPanel panel;  
  private int SizeX;
  private int SizeY;
  
  
  public Gui() {
    
    panel = new JPanel(); 
    
    panel.setLayout(new GridBagLayout());
    GridBagConstraints c = new GridBagConstraints();
    
    c.weighty = 0.5;
    
    int row = 0;
    
    /****************
     * Grid
     ***************/

    c.gridx = 0;
    c.gridy = row;
    c.ipady = 20; 
    panel.add(new JLabel("Grid"), c);
    row++;
    c.ipady = 0;
    
    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Size X: "), c);
      
    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("200       "), c);
    row++;

    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Size Y: "), c);

    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("200       "), c);
    row++;
    
    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Frequency (GHz): "), c);
    
    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("1         "), c);
    row++;

    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("PML Size: "), c);
    
    c.gridx = 1;
    c.gridy = row; 
    panel.add(new JTextField("10         "), c);
    row++;
    
    c.gridx = 0;
    c.gridy = row;
    c.ipady = 20; 
    panel.add(new JLabel(" "), c);
    row++;

    /*****************************
     * Total Field / Scatter Field
     ****************************/
    c.gridx = 0;
    c.gridy = row;
    c.ipady = 20; 
    panel.add(new JLabel("Total Field / Scatter Field"), c);
    c.ipady = 0;
    row++;

    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Angle of Incidence:  "), c);

    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("45        "), c);
    row++;
    
    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Start X: "), c);
      
    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("20         "), c);
    row++;

    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Start Y: "), c);

    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("30        "), c);
    row++;
    
    c.gridx = 0;
    c.gridy = row;
    panel.add(new JLabel("Size: "), c);

    c.gridx = 1;
    c.gridy = row;
    panel.add(new JTextField("150        "), c);
    row++;
    
    /*****************
     * Done Button
     ****************/
    c.gridx = 0;
    c.gridy = row;
    JButton doneButton = new JButton("Done");
    panel.add(doneButton, c);

  }//end constructor 
}//end class













