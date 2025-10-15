*&---------------------------------------------------------------------*
*& Report ZFIR0105
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0105.
DATA: vg_job      TYPE i.
DATA: v_dt_emissao       TYPE /tcsr/t_hd-dtemissao,
      v_dt_emissao_final TYPE /tcsr/t_hd-dtemissao,
      v_ano(4).

TYPES: BEGIN OF ty_nfs,
         doc_contabil_gerado TYPE c.
         INCLUDE STRUCTURE /tcsr/t_hd.
TYPES: END OF ty_nfs.

DATA: it_belnr          TYPE TABLE OF /tcsr/t_hd,
      it_todos          TYPE TABLE OF /tcsr/t_hd,
      it_hd             TYPE TABLE OF /tcsr/t_hd,
      it_nfs            TYPE TABLE OF ty_nfs,
      it_nfs_doc_gerado TYPE TABLE OF ty_nfs,
      it_act            TYPE TABLE OF /tcsr/t_act,
      it_deletar        TYPE TABLE OF /tcsr/t_hd.
DATA: lra_data TYPE RANGE OF char10.

TABLES /tcsr/t_hd.

SELECTION-SCREEN BEGIN OF BLOCK a1.
  SELECT-OPTIONS:
      p_data FOR /tcsr/t_hd-dtemissao NO-EXTENSION.
SELECTION-SCREEN END  OF BLOCK a1.

START-OF-SELECTION.

  IF SY-BATCH EQ ABAP_TRUE.
    TRY.
      ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
    CATCH ZCX_JOB.
    ENDTRY.

    IF E_QTD GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF  p_data[] IS NOT INITIAL.
    APPEND VALUE #( sign = p_data-sign option = p_data-option low = p_data-low high = p_data-high ) TO lra_data.
  ELSEIF vg_job EQ 1 .
    v_dt_emissao = sy-datum - 15.
    APPEND VALUE #( sign = 'I' option = 'BT' low = v_dt_emissao high = sy-datum ) TO lra_data.
  ENDIF.

  CHECK lra_data IS NOT INITIAL.

  SELECT *
    FROM /tcsr/t_act INTO TABLE it_act
    WHERE erdat IN lra_data.

  CHECK it_act[] IS NOT INITIAL.

  SELECT *
    FROM /tcsr/t_hd  INTO CORRESPONDING FIELDS OF TABLE it_hd
     FOR ALL ENTRIES IN it_act
    WHERE guid_header = it_act-guid_header.

  CHECK it_hd[] IS NOT INITIAL.

  SELECT *
    FROM /tcsr/t_hd AS a
     FOR ALL ENTRIES IN @it_hd
   WHERE nfse_numero EQ @it_hd-nfse_numero
     AND p_cnpj      EQ @it_hd-p_cnpj
     AND p_cpf       EQ @it_hd-p_cpf
     AND nfse_year   EQ @it_hd-nfse_year
    INTO CORRESPONDING FIELDS OF TABLE @it_nfs.

  CHECK it_nfs[] is NOT INITIAL.

  SELECT *
    FROM /tcsr/t_hd AS a
     FOR ALL ENTRIES IN @it_hd
   WHERE nfse_numero EQ @it_hd-nfse_numero
     AND p_cnpj      EQ @it_hd-p_cnpj
     AND p_cpf       EQ @it_hd-p_cpf
     AND nfse_year   EQ @it_hd-nfse_year
     AND EXISTS ( SELECT guid_header
                    FROM zibt_nfse_001 AS b
                   WHERE guid_header EQ a~guid_header
                     AND ( b~belnr IS NOT INITIAL OR b~belnr_fret IS NOT INITIAL ) )
    INTO CORRESPONDING FIELDS OF TABLE @it_nfs_doc_gerado.

  LOOP AT it_nfs ASSIGNING FIELD-SYMBOL(<fs_nf>).
    READ TABLE it_nfs_doc_gerado WITH KEY guid_header = <fs_nf>-guid_header TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      <fs_nf>-doc_contabil_gerado = abap_true.
    ENDIF.
  ENDLOOP.

  SELECT *
    FROM /tcsr/t_hd AS a
    FOR ALL ENTRIES IN @it_nfs
    WHERE nfse_numero EQ @it_nfs-nfse_numero
      AND p_cnpj      EQ @it_nfs-p_cnpj
      AND p_cpf       EQ @it_nfs-p_cpf
      AND nfse_year   EQ @it_nfs-nfse_year
      AND NOT EXISTS ( SELECT guid_header
                         FROM zibt_nfse_001 AS b
                        WHERE guid_header EQ a~guid_header
                          AND ( b~belnr IS NOT INITIAL OR b~belnr_fret IS NOT INITIAL )
                      )
   INTO TABLE @it_deletar.

  SORT it_nfs                                    BY nfse_numero p_cnpj p_cpf nfse_year doc_contabil_gerado DESCENDING.
  DELETE  ADJACENT DUPLICATES FROM it_nfs COMPARING nfse_numero p_cnpj p_cpf nfse_year.

  CHECK it_nfs[] IS NOT INITIAL.

  "vai eliminar da comsulta as que tem belnr
  SELECT *
    FROM /tcsr/t_hd AS a
    FOR ALL ENTRIES IN @it_nfs
    WHERE nfse_numero EQ @it_nfs-nfse_numero
      AND p_cnpj      EQ @it_nfs-p_cnpj
      AND p_cpf       EQ @it_nfs-p_cpf
      AND nfse_year   EQ @it_nfs-nfse_year
      AND NOT EXISTS ( SELECT guid_header
                         FROM zibt_nfse_001 AS b
                        WHERE guid_header EQ a~guid_header
                          AND ( b~belnr IS NOT INITIAL OR b~belnr_fret IS NOT INITIAL )
                      )
   INTO TABLE @it_deletar.

  LOOP AT it_nfs INTO DATA(wa_nf).
    LOOP AT it_deletar INTO DATA(wa_deletar) WHERE nfse_numero EQ wa_nf-nfse_numero
                                               AND p_cnpj      EQ wa_nf-p_cnpj
                                               AND p_cpf       EQ wa_nf-p_cpf
                                               AND nfse_year   EQ wa_nf-nfse_year
                                               AND guid_header NE wa_nf-guid_header.

      DELETE FROM /tcsr/t_hd          WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_act         WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_approval    WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_hist        WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_log_del     WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_nfstx       WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_pdf         WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_po          WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_proc_doc    WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_ptfile      WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_ref         WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_sta         WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_wsfile      WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_xml         WHERE guid_header EQ wa_deletar-guid_header.
      DELETE FROM /tcsr/t_xped        WHERE guid_header EQ wa_deletar-guid_header.
    ENDLOOP.
  ENDLOOP.
