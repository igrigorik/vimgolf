Feature: VimGolf Browser
  In order to browse and select VimGolf entries
  As a user
  I want to use the VimGolf browser
  
  Scenario: Press TAB on entry to view description
    Given I open the vimgolf browser
    Then I should not see "Continuing from the last challenge"
    When I press "TAB"
    Then I should see "Continuing from the last challenge"
    When I press "TAB"
    Then I should not see "Continuing from the last challenge"
    
  Scenario: Move through the browse list
    Given I open the vimgolf browser
    Then the cursor should be before "Compl"
    When I press "n"
    Then the cursor should be before "Make "
    When I press "n"
    Then the cursor should be before "Conve"
    When I press "p"
    Then the cursor should be before "Make "
    When I press "p"
    Then the cursor should be before "Compl"
