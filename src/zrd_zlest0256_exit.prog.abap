*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 22/09/2025                                              &*
*& Descrição: Exit Register Data ZLEST0256                            &*
*& Transação: ZLES0223 (Prest. Serv. Frete Terceiros)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |22/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
REPORT zrd_zlest0256_exit.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0001
*&---------------------------------------------------------------------*
*& Prepara a linha de registro ao criar uma nova linha.
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0256 TYPE zlest0256.

  CLEAR: wl_zlest0256.

  wl_zlest0256-dt_registro = sy-datum.
  wl_zlest0256-hr_registro = sy-uzeit.
  wl_zlest0256-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0256 TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0002
*&---------------------------------------------------------------------*
*& Valida dados antes de validar duplicidade de dados e salvar
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0002 USING    p_registro_manter TYPE any
                           CHANGING p_error.

  DATA: wl_zlest0256 TYPE zlest0256.

  CLEAR: wl_zlest0256.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0256.
* Valida e busca textos de Grp. Mercadoria e Material.
  PERFORM zf_busca_valida_grpmerc_mat USING    wl_zlest0256
                                      CHANGING p_error.
* Verifica se deu erro na validação e busca.
  IF NOT p_error IS INITIAL.
    EXIT.

  ENDIF.

  CLEAR: p_error.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0003
*&---------------------------------------------------------------------*
*& Valida dados depois de validar duplicidade de dados e antes de salvar
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0256 TYPE zlest0256.

  DATA: vl_error TYPE c.

  CLEAR: wl_zlest0256.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0256.
* Valida e busca textos de Grp. Mercadoria e Material.
  PERFORM zf_busca_valida_grpmerc_mat USING    wl_zlest0256
                                      CHANGING vl_error.

  wl_zlest0256-dt_registro = sy-datum.
  wl_zlest0256-hr_registro = sy-uzeit.
  wl_zlest0256-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0256 TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0004
*&---------------------------------------------------------------------*
*& Carrega a estrutura da tabela de saída (exibição).
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0004 CHANGING p_saida TYPE any.

  DATA: wl_zlest0256_out TYPE zlest0256_out.

  CLEAR: wl_zlest0256_out.

  MOVE-CORRESPONDING p_saida TO wl_zlest0256_out.

  MOVE-CORRESPONDING wl_zlest0256_out TO p_saida.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0005
*&---------------------------------------------------------------------*
*& Execução antes de gerar a tela (PBO).
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0005 CHANGING p_registro_manter TYPE any.

  LOOP AT SCREEN.
    IF screen-required EQ 1.
      screen-required = 2.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZLEST0256_0016
*&---------------------------------------------------------------------*
*& Dá o giro de validação dos campos na tela de Manutenção/Exibição.
*&---------------------------------------------------------------------*
FORM f_exit_zlest0256_0016 USING    p_ucomm           TYPE sy-ucomm
                           CHANGING p_registro_manter TYPE any
                                    p_saida           TYPE any.

  DATA: wl_zlest0256 TYPE zlest0256.

  DATA: vl_error TYPE c.

  CLEAR: wl_zlest0256.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0256.
* Valida e busca textos de Grp. Mercadoria e Material.
  PERFORM zf_busca_valida_grpmerc_mat USING    wl_zlest0256
                                      CHANGING vl_error.

  IF vl_error IS INITIAL.
    p_registro_manter = CORRESPONDING #( wl_zlest0256 ).
    p_saida           = CORRESPONDING #( wl_zlest0256 ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_busca_valida_grpmerc_mat
*&---------------------------------------------------------------------*
*& Valida e busca textos de Grp. Mercadoria e Material
*&---------------------------------------------------------------------*
*&      --> UE_ZLEST0256 WA Tabela de Parâmetro - Grupo de Mercadoria
*&      <-- CV_ERROR     Variável de validação de erro
*&---------------------------------------------------------------------*
FORM zf_busca_valida_grpmerc_mat USING    ue_zlest0256 STRUCTURE zlest0256
                                 CHANGING cv_error.

  IF ue_zlest0256-matkl IS INITIAL.
* Preencher campo obrigatório "&"
    MESSAGE s278(00) WITH 'Grupo de Material' DISPLAY LIKE 'E'.
    cv_error = abap_on.
    EXIT.

  ELSE.
    IF ue_zlest0256-matnr IS INITIAL.
* Preencher campo obrigatório "&"
      MESSAGE s278(00) WITH 'Material' DISPLAY LIKE 'E'.
      cv_error = abap_on.
      EXIT.

    ENDIF.

  ENDIF.

  SELECT SINGLE wgbez FROM t023t INTO ue_zlest0256-wgbez60 WHERE spras EQ 'P' AND matkl EQ ue_zlest0256-matkl.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE maktx FROM makt INTO ue_zlest0256-maktx WHERE spras EQ 'P' AND matnr EQ ue_zlest0256-matnr.

    IF NOT sy-subrc IS INITIAL.
* O material & não existe ou não foi ativado
      MESSAGE s305(m3) WITH ue_zlest0256-matnr DISPLAY LIKE 'E'.
      cv_error = abap_on.

    ENDIF.

    SELECT SINGLE matkl FROM mara INTO @DATA(vl_matkl) WHERE matnr EQ @ue_zlest0256-matnr
                                                         AND matkl EQ @ue_zlest0256-matkl.

    IF NOT sy-subrc IS INITIAL.
* O material &1 não pertence ao grupo de mercadorias &2.
      MESSAGE s030(w7) WITH ue_zlest0256-matnr ue_zlest0256-matkl DISPLAY LIKE 'E'.
      cv_error = abap_on.

    ENDIF.

  ELSE.
* Grupo de mercadorias &1 não existe.
    MESSAGE s511(cr) WITH ue_zlest0256-matkl DISPLAY LIKE 'E'.
    cv_error = abap_on.

  ENDIF.

ENDFORM.
