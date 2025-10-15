*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 24.06.2024                                              &*
*& Descrição: Validar Acessos transação TK11 / TK12                   &*
*&--------------------------------------------------------------------&*

DATA: g_fcode TYPE char04.

************************************************************************
* limpar parametro VKS quando altera acao fcode
************************************************************************
FORM f_set_parametro_kschl.

  DATA: l_kschl  TYPE rv13a-kschl.

  CHECK sy-tcode = 'TK11' OR
        sy-tcode = 'TK12' OR
        sy-tcode = 'TK13'.

  CLEAR l_kschl.

  SET PARAMETER ID 'VKS' FIELD l_kschl.

ENDFORM.

************************************************************************
* limpa campo KSCHL para refazer o SELECT
************************************************************************
FORM f_limpar_selecao_kschl CHANGING p_kschl.

  CHECK sy-tcode = 'TK11' OR
        sy-tcode = 'TK12' OR
        sy-tcode = 'TK13'.

  CLEAR p_kschl.

ENDFORM.

************************************************************************
* so permitir acesso botao EXIBIR nos programas RV13A*
************************************************************************
FORM f_set_pfstatus TABLES t_excl
                     USING p_kschl.

  DATA: l_erro  TYPE char01.

  CHECK sy-tcode = 'TK11' OR
        sy-tcode = 'TK12' OR
        sy-tcode = 'TK13'.

  CHECK p_kschl IS NOT INITIAL.
  CHECK sy-ucomm <> 'BAC'.

  PERFORM f_veficar_permissao   USING p_kschl
                             CHANGING l_erro.

  IF l_erro = abap_true.
*   APPEND 'AEN1'              TO t_excl.
*   APPEND 'HIZ1'              TO t_excl.
*   APPEND 'HIB1'              TO t_excl.
*
    APPEND fcode_create        TO t_excl.
    APPEND fcode_e_anlegen     TO t_excl.
    APPEND fcode_create_b      TO t_excl.
    APPEND fcode_x_create_b    TO t_excl.
    APPEND fcode_index_search  TO t_excl.
    APPEND fcode_change        TO t_excl.
    APPEND fcode_e_aendern     TO t_excl.
    APPEND fcode_change_b      TO t_excl.
    APPEND fcode_x_change_b    TO t_excl.
    APPEND 'WAEN'              TO t_excl.
    APPEND fcode_create_model  TO t_excl.
    APPEND fcode_e_model       TO t_excl.
    APPEND fcode_create        TO t_excl.
  ENDIF.

ENDFORM.

************************************************************************
* verifica se usuario tem permissao
************************************************************************
FORM f_veficar_permissao   USING p_kschl
                        CHANGING p_erro.

  DATA: t_tvarv    TYPE TABLE OF tvarvc,
        t_usr05    TYPE TABLE OF usr05,
        w_tvarv    TYPE tvarvc,
        w_usr05    TYPE usr05,
        l_uname    TYPE xubname,
        l_param    TYPE char20,
        l_validado TYPE char01.

  p_erro = abap_false.

  CHECK p_kschl IS NOT INITIAL.

  l_uname = sy-uname.

*-Grupo de tipos de condições
  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarv
   WHERE name = 'TK_FISCAL'
      OR name = 'TK_INSUMOS'
      OR name = 'TK_LOGISTICA'
      OR name = 'TK_PAUTA'.     "*-CS2024000778-30.09.2024-#150126-JT-inicio

*-Parametros usuario
  SELECT *
    FROM usr05
    INTO TABLE t_usr05
   WHERE bname = l_uname
     AND parid = 'ZTK'.

*-Valida permissao
  LOOP AT t_usr05 INTO w_usr05.
    CASE w_usr05-parva.
      WHEN 'TK_LOG'.
        l_param = 'TK_LOGISTICA'.
      WHEN 'TK_FIS'.
        l_param = 'TK_FISCAL'.
      WHEN 'TK_INS'.
        l_param = 'TK_INSUMOS'.
      WHEN 'TK_PTA'.
        l_param = 'TK_PAUTA'.   "*-CS2024000778-30.09.2024-#150126-JT-inicio
      WHEN '*'.
        l_validado = abap_true.
        EXIT.
    ENDCASE.

    READ TABLE t_tvarv INTO w_tvarv WITH KEY name = l_param
                                             low  = p_kschl.
    IF sy-subrc = 0.
      l_validado = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  p_erro = COND #( WHEN l_validado = abap_true THEN abap_false
                                               ELSE abap_true ).

ENDFORM.

************************************************************************
* validar acessos e mansagem de erro
************************************************************************
FORM f_validar_acessos USING cts_kappl
                             cts_kvewe
                             cts_kschl.

  DATA: l_erro  TYPE char01,
        l_fcode TYPE t185-fcode.

  FIELD-SYMBOLS: <f_code> TYPE any.

  CHECK sy-tcode = 'TK11' OR
        sy-tcode = 'TK12' OR
        sy-tcode = 'TK13'.

*-----------------------------
*   fcode    acao
*-----------------------------
*   ANZ1     exibir
*   AEN1     modificar
*   HIZ1     criar
*   HIB1     criar com referencia
*   ANTA     combinaca chaves
*   INDE     selecao via indice
*-----------------------------
  ASSIGN ('(SAPMV13A)fcode') TO <f_code>.

  CHECK sy-subrc = 0.

  l_fcode = <f_code>.

  IF l_fcode = 'ANZ1' OR
     l_fcode = 'AEN1' OR
     l_fcode = 'HIZ1' OR
     l_fcode = 'HIB1'.
    g_fcode = l_fcode.
  ENDIF.

  IF g_fcode IS INITIAL.
    CASE sy-tcode.
      WHEN 'TK11'.
        g_fcode = 'HIZ1'.
      WHEN 'TK12'.
        g_fcode = 'AEN1'.
      WHEN 'TK13'.
        g_fcode = 'ANZ1'.
    ENDCASE.
  ENDIF.

  IF g_fcode = 'ANZ1'.
    EXIT.
  ENDIF.

  PERFORM f_veficar_permissao   USING cts_kschl
                             CHANGING l_erro.

  IF l_erro = abap_true.
    MESSAGE e024(sd) WITH 'Usuário não autorizado a realizar' ' lançamentos nessa transação!'.
  ENDIF.

ENDFORM.

************************************************************************
************************************************************************
