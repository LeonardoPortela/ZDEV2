"Name: \PR:HBRSALF0\FO:EMPLOYEE_SELECTION\SE:BEGIN\EI
ENHANCEMENT 0 ZHCMR0017_SALARIO_FAMI_001.
*

  DATA:  lc_nr_depend       TYPE nr_depend.
  CLEAR: lc_nr_depend.

* Check employee's dependents for family allowance
  PERFORM fill_dependent  USING wa_header
                       CHANGING lc_nr_depend.

* if there are valid dependents, get the value of family allowance
* and finally append the header
  IF lc_nr_depend NE 0.
    PERFORM read_t7brsf USING wa_header
                              pn-begda
                              pn-endda.
    APPEND wa_header TO header.
  else.
    APPEND wa_header TO header.
  ENDIF.

  CHECK 1 eq 2.

ENDENHANCEMENT.
