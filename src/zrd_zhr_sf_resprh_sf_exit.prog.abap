*&---------------------------------------------------------------------*
*& Report  ZRD_zhr_sf_rhrem_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zhr_sf_resprh_sf_exit.

*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_resprh_sf_0001 CHANGING p_registro_manter TYPE any.

  DATA: w_zhr_sf_resprh_sf_out TYPE zhr_sf_resprh_sf_out,
        l_cpf                  TYPE c LENGTH 14.

  CLEAR: w_zhr_sf_resprh_sf_out.


  IF w_zhr_sf_resprh_sf_out-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zhr_sf_resprh_sf_out-cpf
      IMPORTING
        output = l_cpf.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE pernr, cpf_nr, endda FROM pa0465
      INTO @DATA(w_pa0465)
      WHERE cpf_nr = @l_cpf
        AND endda >= @sy-datum.

    IF w_pa0465-pernr IS NOT INITIAL.

      "Registro mestre HR infotipo 0002 (Dados pessoais)
      SELECT SINGLE cname FROM pa0002
        INTO (w_zhr_sf_resprh_sf_out-nome)
        WHERE  pernr = w_pa0465-pernr
           AND endda >= sy-datum.

    ENDIF.

  ENDIF.

  "---------------------------------------------------------------------
  " Área
  "---------------------------------------------------------------------
  IF w_zhr_sf_resprh_sf_out-area IS NOT INITIAL.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE atext FROM t549t
      INTO (w_zhr_sf_resprh_sf_out-descricao)
      WHERE sprsl = sy-langu(1)
        AND abkrs = w_zhr_sf_resprh_sf_out-area.

  ENDIF.

  w_zhr_sf_resprh_sf_out-usuario  = sy-uname.
  w_zhr_sf_resprh_sf_out-data     = sy-datum.
  w_zhr_sf_resprh_sf_out-hora     = sy-uzeit.

  MOVE-CORRESPONDING w_zhr_sf_resprh_sf_out TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_resprh_sf_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: w_zhr_sf_resprh_sf_out TYPE zhr_sf_resprh_sf_out,
        l_cpf                  TYPE c LENGTH 14.

  CLEAR: w_zhr_sf_resprh_sf_out.
  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_resprh_sf_out.

  "---------------------------------------------------------------------
  " Área
  "---------------------------------------------------------------------
  IF w_zhr_sf_resprh_sf_out-area IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Área proc.FlhPagto é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF w_zhr_sf_resprh_sf_out-area IS NOT INITIAL.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE atext FROM t549t
      INTO (w_zhr_sf_resprh_sf_out-descricao)
      WHERE sprsl = sy-langu(1)
        AND abkrs = w_zhr_sf_resprh_sf_out-area.

  ENDIF.

  IF w_zhr_sf_resprh_sf_out-descricao IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Área proc.FlhPagto não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  "---------------------------------------------------------------------
  " Valida dados do CPF
  "---------------------------------------------------------------------
  IF w_zhr_sf_resprh_sf_out-cpf IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cpf é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF w_zhr_sf_resprh_sf_out-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zhr_sf_resprh_sf_out-cpf
      IMPORTING
        output = l_cpf.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE pernr, cpf_nr, endda FROM pa0465
      INTO @DATA(w_pa0465)
      WHERE cpf_nr = @l_cpf
        AND endda >= @sy-datum.

    IF w_pa0465-pernr IS NOT INITIAL.

      "Registro mestre HR infotipo 0002 (Dados pessoais)
      SELECT SINGLE cname FROM pa0002
        INTO (w_zhr_sf_resprh_sf_out-nome)
        WHERE  pernr = w_pa0465-pernr
           AND endda >= sy-datum.

    ENDIF.

  ENDIF.

  IF w_zhr_sf_resprh_sf_out-nome IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cpf não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zhr_sf_resprh_sf_out TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_resprh_sf_0003 CHANGING p_registro_manter TYPE any.

  DATA: w_zhr_sf_resprh_sf_out TYPE zhr_sf_resprh_sf_out.

  CLEAR: w_zhr_sf_resprh_sf_out.
  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_resprh_sf_out.

  w_zhr_sf_resprh_sf_out-data     = sy-datum.
  w_zhr_sf_resprh_sf_out-usuario  = sy-uname.
  w_zhr_sf_resprh_sf_out-hora     = sy-uzeit.

  MOVE-CORRESPONDING w_zhr_sf_resprh_sf_out TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Preenche campos de leitura
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_resprh_sf_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zhr_sf_resprh_sf_out TYPE zhr_sf_resprh_sf_out,
        l_cpf                  TYPE c LENGTH 14.

  CLEAR: w_zhr_sf_resprh_sf_out.
  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_resprh_sf_out.

  w_zhr_sf_resprh_sf_out-data     = sy-datum.
  w_zhr_sf_resprh_sf_out-usuario  = sy-uname.
  w_zhr_sf_resprh_sf_out-hora     = sy-uzeit.

  IF w_zhr_sf_resprh_sf_out-area IS NOT INITIAL.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE atext FROM t549t
      INTO (w_zhr_sf_resprh_sf_out-descricao)
      WHERE sprsl = sy-langu(1)
        AND abkrs = w_zhr_sf_resprh_sf_out-area.

  ENDIF.

  IF w_zhr_sf_resprh_sf_out-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zhr_sf_resprh_sf_out-cpf
      IMPORTING
        output = l_cpf.

    "Registro mestre HR infotipo 0465
    SELECT SINGLE pernr, cpf_nr, endda FROM pa0465
      INTO @DATA(w_pa0465)
      WHERE cpf_nr = @l_cpf
        AND endda >= @sy-datum.

    IF w_pa0465-pernr IS NOT INITIAL.

      "Registro mestre HR infotipo 0002 (Dados pessoais)
      SELECT SINGLE cname FROM pa0002
        INTO (w_zhr_sf_resprh_sf_out-nome)
        WHERE  pernr = w_pa0465-pernr
           AND endda >= sy-datum.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING w_zhr_sf_resprh_sf_out TO p_registro_manter.

ENDFORM.
