*&---------------------------------------------------------------------*
*& Report  ZRD_zhr_sf_rhrem_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zhr_sf_rhrem_exit.

*&---------------------------------------------------------------------*
*&  "Set atributos iniciais ao incluir um novo registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_rhrem_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zhr_sf_rhrem TYPE zhr_sf_rhrem_out.

  CLEAR: wl_zhr_sf_rhrem.

  wl_zhr_sf_rhrem-usuario  = sy-uname.
  wl_zhr_sf_rhrem-data     = sy-datum.
  wl_zhr_sf_rhrem-hora     = sy-uzeit.

  MOVE-CORRESPONDING wl_zhr_sf_rhrem TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Validações antes da gravação do registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_rhrem_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

* Declaração de Estruturas
  DATA: w_zhr_sf_rhrem TYPE zhr_sf_rhrem_out,
        l_cpf          TYPE c LENGTH 14.

  CLEAR: w_zhr_sf_rhrem.

  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_rhrem.

* Busca Nome da pessoa
  IF w_zhr_sf_rhrem-cpf IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cpf é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF w_zhr_sf_rhrem-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zhr_sf_rhrem-cpf
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
        INTO (w_zhr_sf_rhrem-nome)
        WHERE  pernr = w_pa0465-pernr
           AND endda >= sy-datum.

    ENDIF.

  ENDIF.

  IF w_zhr_sf_rhrem-nome IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Cpf não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zhr_sf_rhrem TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Set atributos antes de gravar o registro
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_rhrem_0003 CHANGING p_registro_manter TYPE any.

  DATA: w_zhr_sf_rhrem TYPE zhr_sf_rhrem_out.

  CLEAR: w_zhr_sf_rhrem.
  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_rhrem.

  w_zhr_sf_rhrem-data     = sy-datum.
  w_zhr_sf_rhrem-usuario  = sy-uname.
  w_zhr_sf_rhrem-hora     = sy-uzeit.

  MOVE-CORRESPONDING w_zhr_sf_rhrem TO p_registro_manter.

ENDFORM.

*&---------------------------------------------------------------------*
*&    "Preenche campos de leitura
*&---------------------------------------------------------------------*
FORM f_exit_zhr_sf_rhrem_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zhr_sf_rhrem TYPE zhr_sf_rhrem_out,
        l_cpf          TYPE c LENGTH 14.

  CLEAR: w_zhr_sf_rhrem.
  MOVE-CORRESPONDING p_registro_manter TO w_zhr_sf_rhrem.

  w_zhr_sf_rhrem-data     = sy-datum.
  w_zhr_sf_rhrem-usuario  = sy-uname.
  w_zhr_sf_rhrem-hora     = sy-uzeit.

* Busca Nome da pessoa
  IF w_zhr_sf_rhrem-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zhr_sf_rhrem-cpf
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
        INTO (w_zhr_sf_rhrem-nome)
        WHERE  pernr = w_pa0465-pernr
           AND endda >= sy-datum.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING w_zhr_sf_rhrem TO p_registro_manter.

ENDFORM.
