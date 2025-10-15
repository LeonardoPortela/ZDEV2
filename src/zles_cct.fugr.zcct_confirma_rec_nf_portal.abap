FUNCTION zcct_confirma_rec_nf_portal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_JOB) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(I_CHAVES) TYPE  ZDE_CHAVE_DOC_E_T
*"     REFERENCE(I_FORCE_CONSULTA) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_CONSULTA_REALIZADA) TYPE  CHAR01
*"     REFERENCE(E_ERRO_AUTENTICACAO) TYPE  CHAR01
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_chaves,
           chave TYPE c LENGTH 44,
         END OF ty_chaves.

  DATA: it_nfe_cons          TYPE zde_chave_doc_e_t,
        wl_nfe_cons          TYPE zde_chave_doc_e,
        tg_zlest0186         TYPE TABLE OF zlest0186 WITH HEADER LINE,
        tg_chaves            TYPE TABLE OF ty_chaves WITH HEADER LINE,
        wl_estoque_nf_cct_js TYPE zde_estoque_nf_cct_js,
        v_lim_cons_nf        TYPE j_1bnflin-menge,
        vg_filial            TYPE werks_d,
        gv_job               TYPE char1.

  DATA: zcl_cct_recepcao_carga  TYPE REF TO zcl_cct_recepcao_carga,
        zcl_token_siscomex_0001 TYPE REF TO zcl_token_siscomex.

  CLEAR: tg_zlest0186[].

  CLEAR: e_consulta_realizada, e_erro_autenticacao, gv_job.

  IF i_job IS NOT INITIAL.
    gv_job = 'X'.
  ENDIF.


  LOOP AT i_chaves INTO DATA(wl_chave).
    tg_chaves-chave = wl_chave.
    APPEND tg_chaves.
  ENDLOOP.

  DELETE tg_chaves WHERE chave IS INITIAL.

  SORT tg_chaves BY chave.
  DELETE ADJACENT DUPLICATES FROM tg_chaves COMPARING chave.

  CHECK tg_chaves[] IS NOT INITIAL.

  "Limite de notas por Consulta
  SELECT SINGLE *
    FROM setleaf INTO @DATA(_wl_setleaf_lim_nf)
   WHERE setname EQ 'ZCCT_LIMITE_CONS_NF'.

  IF ( sy-subrc EQ 0 ).
    v_lim_cons_nf = _wl_setleaf_lim_nf-valfrom.
  ELSE.
    v_lim_cons_nf = 100.
  ENDIF.

  IF i_force_consulta EQ abap_false.
    "Consultar Confirmações já gravadas...
    SELECT *
      FROM zlest0186 INTO TABLE tg_zlest0186
       FOR ALL ENTRIES IN tg_chaves
     WHERE chave = tg_chaves-chave.

    "Deleta notas já consultadas
    LOOP AT tg_chaves INTO wl_chave.
      READ TABLE tg_zlest0186 WITH KEY chave = wl_chave.
      IF sy-subrc EQ 0.
        DELETE tg_chaves.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CHECK tg_chaves[] IS NOT INITIAL.

*---------------------------------------------------------------*
* Autenticar no Portal
*---------------------------------------------------------------*
  CLEAR: vg_filial.
  FREE zcl_token_siscomex_0001.
  CREATE OBJECT zcl_token_siscomex_0001.

"======================================IR149136 / AOENNING
  "Limite de notas por Consulta
  SELECT SINGLE *
    FROM setleaf INTO @DATA(wl_filial)
   WHERE setname EQ 'ZCCT_FILIAL_TOKEN'.
  IF sy-subrc EQ 0.
    vg_filial = wl_filial-valfrom.
  ELSE.
    vg_filial = '0001'.
  ENDIF.
"======================================IR149136 / AOENNING

  zcl_token_siscomex_0001->zif_cadastro~novo_registro( ).
  zcl_token_siscomex_0001->set_bukrs( vg_filial ).
  zcl_token_siscomex_0001->set_role_type( 'IMPEXP' ). "Declarante importador/exportador
  zcl_token_siscomex_0001->zif_cadastro~gravar_registro( EXPORTING i_job = gv_job  RECEIVING i_gravou = DATA(_gravou)  ). "Se caso for rodado via job.


  "zcl_token_siscomex_0001->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

  IF _gravou EQ abap_false.
    e_erro_autenticacao = abap_true.
    RETURN.
  ENDIF.

  CLEAR: it_nfe_cons[].

  LOOP AT tg_chaves INTO wl_chave.

    DATA(_tabix) = sy-tabix.

    wl_nfe_cons = wl_chave.
    APPEND wl_nfe_cons TO it_nfe_cons.

    "Consultar em lotes de 100 notas            "Ultima Nota
    IF ( lines( it_nfe_cons[] ) >= v_lim_cons_nf ) OR ( _tabix EQ lines( tg_chaves[] ) )  .

      FREE zcl_cct_recepcao_carga.
      CREATE OBJECT zcl_cct_recepcao_carga.

      zcl_cct_recepcao_carga->set_token( zcl_token_siscomex_0001 ).

      TRY.
          zcl_cct_recepcao_carga->consultar_estoque_pre_acd(
            EXPORTING
              i_chaves_nfe        = it_nfe_cons
            IMPORTING
              e_estoque_nf_cct_js = wl_estoque_nf_cct_js
          ).

          CLEAR: it_nfe_cons[].

          PERFORM f_proc_ret_cons_pre_acd USING wl_estoque_nf_cct_js.

        CATCH zcx_cct INTO DATA(ex_cct).
          ex_cct->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    ENDIF.

  ENDLOOP.


  e_consulta_realizada = abap_true.

ENDFUNCTION.
