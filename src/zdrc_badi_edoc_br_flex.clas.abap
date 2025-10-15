class ZDRC_BADI_EDOC_BR_FLEX definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EDOC_BR_FLEX .
protected section.
private section.
ENDCLASS.



CLASS ZDRC_BADI_EDOC_BR_FLEX IMPLEMENTATION.


  METHOD if_edoc_br_flex~step_after_dacte ##NEEDED.

    DATA: l_chave   TYPE edoc_accesskey,
          w_bapiret TYPE bapiret2.

    FREE: bapiret.

    l_chave = io_cte->retrieve_accesskey( ).

    SELECT SINGLE *
      FROM edobrcteincoming
      INTO @DATA(w_edoc)
     WHERE accesskey = @l_chave.

    CHECK sy-subrc = 0.

*   w_bapiret-type    = 'E'.
*   w_bapiret-id      = 'SD'.
*   w_bapiret-number  = '024'.
*   w_bapiret-message = 'Falta decisão do usuário para o CTe.'.
*
*   APPEND w_bapiret TO bapiret.

  ENDMETHOD.


  method IF_EDOC_BR_FLEX~STEP_AFTER_DACTEOS ##NEEDED.
  endmethod.


  method IF_EDOC_BR_FLEX~STEP_BEFORE_DACTE ##NEEDED.
  endmethod.


  method IF_EDOC_BR_FLEX~STEP_BEFORE_DACTEOS ##NEEDED.
  endmethod.
ENDCLASS.
