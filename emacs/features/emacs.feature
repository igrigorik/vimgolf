Feature: VimGolf Browser
  In order to browse and select VimGolf entries
  As a user
  I want to use the VimGolf browser
  
  Scenario: Press TAB on entry to view description
    Given I open the vimgolf browser
    And I go to line "3"
    Then I should not see "Continuing from the last challenge"
    When I press "TAB"
    Then I should see "Continuing from the last challenge"
    When I press "TAB"
    Then I should not see "Continuing from the last challenge"
    
