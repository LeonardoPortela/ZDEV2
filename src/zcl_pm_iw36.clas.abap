class ZCL_PM_IW36 definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_EAM_SINGLELEVELLIST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PM_IW36 IMPLEMENTATION.


  method IF_EX_BADI_EAM_SINGLELEVELLIST~CHANGE_FIELD_CATALOG.

    IF SY-SUBRC EQ 0.

    ENDIF.
  endmethod.


  METHOD if_ex_badi_eam_singlelevellist~fill_add_fields.

    DATA: vg_equnr      TYPE equnr,
          vg_value_vida TYPE imrc_cntrc,
          vg_value      TYPE imrc_cntrc.

    FIELD-SYMBOLS: <baujj>           TYPE any,
                   <equnr>           TYPE any, "
                   <zzvida_util>     TYPE any,
                   <zzposition_cont> TYPE any,
                   <zzidade_eqpto>   TYPE any.



* assign de estrutura com o nome do campo da estrutura
    ASSIGN COMPONENT 'BAUJJ' OF STRUCTURE cs_object
                                       TO <baujj>.


    IF <baujj> IS ASSIGNED.
      IF <baujj> IS NOT INITIAL.

        ASSIGN COMPONENT 'ZZIDADE_EQPTO' OF STRUCTURE cs_object
                                         TO <zzidade_eqpto>.

        IF <zzidade_eqpto> IS ASSIGNED.
          <zzidade_eqpto> = sy-datum(4) - <baujj>.
          CONDENSE <zzidade_eqpto> NO-GAPS.
        ENDIF.
      ENDIF.
    ENDIF.


* assign de estrutura com o nome do campo equipamento.
    ASSIGN COMPONENT 'EQUNR' OF STRUCTURE cs_object
                                       TO <equnr>.
    "Pegar ultima posição contador.

    IF <equnr> IS ASSIGNED.
      IF <equnr> IS NOT INITIAL.

        ASSIGN COMPONENT 'ZZVIDA_UTIL' OF STRUCTURE cs_object
                                         TO <zzvida_util>.

        ASSIGN COMPONENT 'ZZPOSITION_CONT' OF STRUCTURE cs_object
                                         TO <zzposition_cont>.

        IF <zzposition_cont> IS ASSIGNED.
          TRY .
              CLEAR: vg_equnr, vg_value.
              vg_equnr = CONV #( <equnr> ).
              CALL FUNCTION 'ZPM_GET_POSITION_CONT'
                EXPORTING
                  i_equnr     = vg_equnr
                IMPORTING
                  e_vida_util = vg_value_vida
                  e_value     = vg_value.

              IF vg_value IS NOT INITIAL.
                REPLACE '.' IN vg_value WITH ','.
                <zzposition_cont> = vg_value.
              ENDIF.

              IF vg_value_vida IS NOT INITIAL.
                REPLACE '.' IN vg_value_vida WITH ','.
                <zzvida_util> = vg_value_vida.
              ENDIF.

            CATCH cx_sy_dyn_call_illegal_func INTO DATA(ws_error).

          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
