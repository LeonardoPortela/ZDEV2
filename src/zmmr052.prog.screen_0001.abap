PROCESS BEFORE OUTPUT.
  MODULE main_output.

  CALL SUBSCREEN employee
    INCLUDING
      sy-repid
      employee_block-subscreen.

  CALL SUBSCREEN classification
    INCLUDING
      sy-repid
      classification_block-subscreen.

  CALL SUBSCREEN itens
    INCLUDING
      sy-repid
      itens_block-subscreen.

PROCESS AFTER INPUT.
  CALL SUBSCREEN employee.
  CALL SUBSCREEN classification.

  MODULE main_input.
