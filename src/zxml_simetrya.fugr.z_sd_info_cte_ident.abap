FUNCTION z_sd_info_cte_ident.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_CTE_GERA) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(P_IDENTIFICA) TYPE  ZCTE_IDENTIFICA
*"  TABLES
*"      P_PARCEIROS STRUCTURE  J_1BNFNAD OPTIONAL
*"      P_T001W STRUCTURE  T001W OPTIONAL
*"  CHANGING
*"     VALUE(P_DOC_TRANSP) TYPE  VTTK OPTIONAL
*"  EXCEPTIONS
*"      N_PLACA_CAD
*"----------------------------------------------------------------------

  DATA: wa_j_1bnfdoc     TYPE j_1bnfdoc,
        wa_j_1bnfdoc_nf  TYPE j_1bnfdoc,
        wa_j_1bnflin     TYPE j_1bnflin,
        wa_vkrk          TYPE vbrk,
        vg_ztrem         TYPE dzterm,
        wa_parceiros     TYPE j_1bnfnad,
        wa_lfa1          TYPE lfa1,
        wa_kna1          TYPE kna1,
        wa_t001w         TYPE t001w,
*        WA_TVRO      TYPE TVRO,
        wa_emissor       TYPE lfa1,
        wa_zlest0061     TYPE zlest0061,
        cidade           TYPE j_1btxjurt,
        wa_j_1bnfstx     TYPE j_1bnfstx,
        vl_kunnr_ag      TYPE j_1bnfnad-parid,
        vl_kunnr_rg      TYPE j_1bnfnad-parid,
        vl_cte_strt_lct  TYPE j_1bnfdoc-cte_strt_lct,
        vl_cte_end_lct   TYPE j_1bnfdoc-cte_end_lct,
        it_vfkp          TYPE TABLE OF vfkp INITIAL SIZE 0 WITH HEADER LINE,
        wa_vfkp          TYPE vfkp,
        it_konv          TYPE TABLE OF konv INITIAL SIZE 0 WITH HEADER LINE,
        it_cte_info_nota TYPE TABLE OF zcte_info_nota WITH HEADER LINE,
        wa_konv          TYPE konv,
        vg_kinak         LIKE konv-kinak,
        vl_aliq          TYPE konv-kwert,
        vl_vicms         TYPE vfkp-kzwi6,
        vl_bicm          TYPE konv-kwert,
        "vl_bicm      TYPE konv-kawrt,
        vg_numero        LIKE zcte_identifica-nct,
        taxjurcode       TYPE j_1btxjcd,
        p_parid          TYPE j_1bparid,
        p_parid_we       TYPE j_1bparid,
        p_parid_rg       TYPE j_1bparid,
        vl_taxsit        TYPE c LENGTH 2,
        wa_zfiwrt0008    TYPE zfiwrt0008,
        wa_ekko          TYPE ekko,
        wa_vttp          TYPE vttp,
        wa_vbpa          TYPE vbpa,
        wa_zlest0026     TYPE zlest0026,
        t_cte_trans      TYPE TABLE OF zcte_trans,    "*-CS2023000780-20.09.2024-JT-#127476-inicio
        it_prcd_elem     TYPE TABLE OF prcd_elements. "*-CS2023000780-20.09.2024-JT-#127476-inicio
*       var_versao_xml_tip TYPE char10.

* Calcular a data de entrega
  DATA: vl_quociente  TYPE i,
        vl_dias(10)   TYPE c,
        vl_dias_n(10) TYPE n.


  IF p_cte_gera IS INITIAL.

    SELECT SINGLE * INTO p_identifica
      FROM zcte_identifica
     WHERE docnum EQ p_cte_avulso.

    IF ( p_identifica-nct IS INITIAL ) AND ( sy-subrc IS INITIAL ).

      SELECT SINGLE * INTO wa_j_1bnfdoc
        FROM j_1bnfdoc
       WHERE docnum EQ p_cte_avulso.

      IF sy-subrc IS INITIAL.

        IF wa_j_1bnfdoc-nfe EQ 'X'.
          vg_numero = wa_j_1bnfdoc-nfenum.
        ELSE.
          vg_numero = wa_j_1bnfdoc-nfnum.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vg_numero
          IMPORTING
            output = p_identifica-nct.
      ENDIF.
      MODIFY zcte_identifica FROM p_identifica.


    ENDIF.

  ELSE.

    CALL FUNCTION 'Z_SD_INFO_CTE_TRANS'
      EXPORTING
        p_cte_avulso = p_cte_avulso
        p_cte_gera   = c_x
      IMPORTING
        p_doc_transp = p_doc_transp
      TABLES
        it_cte_trans = t_cte_trans  "*-CS2023000780-20.09.2024-JT-#127476-inicio
      EXCEPTIONS
        n_placa_cad  = 1
        OTHERS       = 2.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e026 WITH sy-msgv1 RAISING n_placa_cad.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

    p_identifica-docnum = p_cte_avulso.
    p_identifica-tknum  = p_doc_transp-tknum.

    SELECT SINGLE * INTO wa_zlest0026
      FROM zlest0026
     WHERE tknum EQ p_doc_transp-tknum.

    p_identifica-nr_vr_xml_tipf = wa_zlest0026-nr_vr_xml_tipf.
    p_identifica-tp_admim_ped   = wa_zlest0026-tp_admim_ped.
    p_identifica-tp_admim_frete = wa_zlest0026-tp_admim_frete.

    IF sy-subrc IS INITIAL AND wa_zlest0026-tp_card_ped IS NOT INITIAL.

      p_identifica-tp_card_ped    = wa_zlest0026-tp_card_ped.
      p_identifica-nr_card_ped    = wa_zlest0026-nr_card_ped.
      p_identifica-id_rota        = wa_zlest0026-id_rota.
      p_identifica-qtd_eixo       = wa_zlest0026-qtd_eixo.

    ELSEIF sy-subrc IS NOT INITIAL.

      "Para casos onde não existe pedágio pegar a versão da XML da Filial
      SELECT SINGLE nr_vr_xml_tipf FROM zlest0090 INTO p_identifica-nr_vr_xml_tipf WHERE werks EQ p_doc_transp-tplst AND ck_default EQ abap_true.

**=============================================Inicio les - ajuste montagem XML cte #ir175016 aoenning
* Este trecho estava na request abaixo:
*    DEVK9A1X7Q       ABAP         LES - Ajuste montagem XML CTE #IR175016 AOENNING
*
*      "Stvarv versão XML/TIP.
*      IF p_identifica-nr_vr_xml_tipf IS INITIAL.
*        CLEAR: var_versao_xml_tip.
*        SELECT SINGLE low FROM tvarvc
*        INTO var_versao_xml_tip
*        WHERE name = 'Z_VERSAO_XML_TIP'.
*        IF sy-subrc EQ 0.
*          p_identifica-nr_vr_xml_tipf = var_versao_xml_tip.
*        ELSE.
*          p_identifica-nr_vr_xml_tipf = '1.17'.
*        ENDIF.
*      ENDIF.

      IF p_identifica-nr_vr_xml_tipf IS INITIAL.
        p_identifica-nr_vr_xml_tipf = '1.15'.
      ENDIF.
**=============================================fim les - ajuste montagem XML cte #ir175016 aoenning
    ENDIF.

*-CS2024001181-16.12.2024-#160717-JT-inicio
    IF p_identifica-nr_tag_strada IS INITIAL.
      TRY.
          p_identifica-nr_tag_strada = t_cte_trans[ tp_veiculo = '0' ]-nr_tag_strada.
        CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
      ENDTRY.
    ENDIF.
*-CS2024001181-16.12.2024-#160717-JT-fim

    IF p_parceiros IS INITIAL.

      CALL FUNCTION 'Z_SD_INFO_CTE_NOTAS'
        EXPORTING
          p_cte_avulso     = p_cte_avulso
          p_cte_gera       = c_x
        TABLES
          it_cte_info_nota = it_cte_info_nota
          it_nf_partner    = p_parceiros
        EXCEPTIONS
          sem_notas        = 1
          OTHERS           = 2.

    ENDIF.

    SELECT SINGLE * INTO wa_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ p_cte_avulso.

    SELECT SINGLE * INTO wa_j_1bnflin
      FROM j_1bnflin
     WHERE docnum EQ p_cte_avulso.

    p_identifica-dhemi  = wa_j_1bnfdoc-credat.
    p_identifica-hremi  = wa_j_1bnfdoc-cretim.

    "Ini. CS2017002043 04.10.2017
    IF ( wa_j_1bnfdoc-docnum IS NOT INITIAL ).
      DATA: v_time_br TYPE erzet.
      CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
        EXPORTING
          i_bukrs  = wa_j_1bnfdoc-bukrs
          i_branch = wa_j_1bnfdoc-branch
        IMPORTING
          e_time   = v_time_br.
      IF v_time_br IS NOT INITIAL.
        p_identifica-hremi = v_time_br.
      ENDIF.
    ENDIF.
    "Fim. CS2017002043 04.10.2017

    p_identifica-serie  = wa_j_1bnfdoc-series.
    p_identifica-nct    = wa_j_1bnfdoc-nfenum.
    p_identifica-cfop   = wa_j_1bnflin-cfop.

    SELECT SINGLE cfotxt INTO p_identifica-cfotxt
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ wa_j_1bnflin-cfop.

    IF p_doc_transp-shtyp = c_z020.
      p_identifica-forpag = c_1.
    ELSEIF p_doc_transp-shtyp NE c_z026.
      SELECT SINGLE * INTO wa_vkrk
        FROM vbrk
       WHERE vbeln EQ wa_j_1bnflin-refkey(10).

      SELECT SINGLE zterm INTO vg_ztrem
        FROM t052
       WHERE zterm = wa_vkrk-zterm
         AND zstg1 NE space.

      IF sy-subrc IS INITIAL.
        p_identifica-forpag = c_1.
      ELSE.
        p_identifica-forpag = c_0.
      ENDIF.
    ENDIF.

    IF wa_j_1bnfdoc-doctyp = '2'. "Complementar.
      p_identifica-tpcte   = c_1.
    ELSE.
      p_identifica-tpcte   = c_0.
    ENDIF.

    p_identifica-tpserv	 = c_0.
    p_identifica-retira	 = c_1.

    CASE p_doc_transp-vsart.
      WHEN '01'.
        p_identifica-modal = c_01.
      WHEN '02'.
        p_identifica-modal = c_04.
      WHEN '03' OR '04'.
        p_identifica-modal = c_03.
    ENDCASE.

    IF ( p_identifica-modal IS INITIAL ).
      SELECT SINGLE * FROM zlest0061 INTO wa_zlest0061 WHERE docnum EQ p_cte_avulso.
      IF ( sy-subrc EQ 0 ).
        p_identifica-modal = c_03.
      ENDIF.
    ENDIF.

    CASE p_identifica-modal.
      WHEN c_01.
        p_identifica-rodo_frete_lota = c_1.

        p_identifica-emissor        = p_doc_transp-tdlnr.
        p_identifica-rodo_dt_inicio = p_doc_transp-datbg.
        SELECT SINGLE * INTO wa_emissor FROM lfa1 WHERE lifnr = p_doc_transp-tdlnr.
        IF sy-subrc IS INITIAL.
          p_identifica-rodo_rntrc = wa_emissor-bahns.
        ENDIF.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = wa_j_1bnfdoc-docdat
            days      = 30
            months    = 0
            years     = 0
          IMPORTING
            calc_date = p_identifica-rodo_dt_prev.

*        SELECT SINGLE * INTO WA_TVRO FROM TVRO WHERE ROUTE = P_DOC_TRANSP-ROUTE.
*
*        IF SY-SUBRC IS INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_TSTRG_OUTPUT'
*            EXPORTING
*              INPUT  = WA_TVRO-TRAZTD
*            IMPORTING
*              OUTPUT = VL_DIAS.
*
*          VL_DIAS_N = VL_DIAS.
*          VL_DIAS_N = VL_DIAS_N / 100.
*          VL_QUOCIENTE = VL_DIAS_N DIV 24.
*          VG_DTA_ENT = P_DOC_TRANSP-DATBG + VL_QUOCIENTE.
*          P_IDENTIFICA-RODO_DT_PREV = VG_DTA_ENT.
*        ENDIF.
    ENDCASE.

    p_identifica-spras   = 'P'.
    p_identifica-country = 'BR'.

* Início da Prestação do serviço
    IF ( p_doc_transp-abfer = c_1 OR p_doc_transp-abfer = c_3 ) AND ( p_doc_transp-shtyp NE c_z026 AND
*-CS2021001045 - 08.03.2022 - JT - inicio
         p_doc_transp-shtyp NE c_z005 ).
*-CS2021001045 - 08.03.2022 - JT - fim
      READ TABLE p_parceiros INTO wa_parceiros WITH KEY parvw  = c_pc.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_parceiros-parid.
      ENDIF.
    ELSEIF ( p_doc_transp-abfer = c_2 OR p_doc_transp-abfer = c_4 ) AND ( p_doc_transp-shtyp NE c_z021 ).
      READ TABLE p_parceiros INTO wa_parceiros WITH KEY parvw  = c_lf.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_parceiros-parid.
      ENDIF.
    ELSEIF ( p_doc_transp-abfer = c_2 ) AND ( p_doc_transp-shtyp EQ c_z021 ).

      READ TABLE it_cte_info_nota INDEX 1.

      IF ( it_cte_info_nota[] IS INITIAL ) OR ( it_cte_info_nota-docnum_nf IS INITIAL ).
        SELECT SINGLE * FROM vttp INTO wa_vttp WHERE tknum EQ p_doc_transp-tknum.
        IF ( sy-subrc EQ 0 ).
          SELECT SINGLE * FROM vbpa INTO wa_vbpa WHERE vbeln EQ wa_vttp-vbeln
                                                   AND parvw EQ 'PC'.
          SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_vbpa-lifnr.
        ENDIF.
      ELSE.
        READ TABLE it_cte_info_nota INDEX 1.

        "Modificação para pegar o código do Municipio Inicio.
        SELECT SINGLE * FROM zfiwrt0008 INTO wa_zfiwrt0008 WHERE docnum EQ it_cte_info_nota-docnum_nf.

        IF ( sy-subrc EQ 0 ) AND ( wa_zfiwrt0008-ebeln IS NOT INITIAL ).
          SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln EQ wa_zfiwrt0008-ebeln.
          SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_ekko-lifnr.

          CASE wa_lfa1-ktokk.
            WHEN: 'ZFEX'.
              SELECT SINGLE * FROM vttp INTO wa_vttp WHERE tknum EQ p_doc_transp-tknum.
              IF ( sy-subrc EQ 0 ).
                SELECT SINGLE * FROM vbpa INTO wa_vbpa WHERE vbeln EQ wa_vttp-vbeln
                                                         AND parvw EQ 'PC'.
                SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_vbpa-lifnr.
              ENDIF.
          ENDCASE.
        ELSE.
          SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = it_cte_info_nota-cliente.
        ENDIF.
      ENDIF.

    ELSEIF ( p_doc_transp-abfer = c_1 ) AND ( p_doc_transp-shtyp EQ c_z026 OR
*-CS2021001045 - 08.03.2022 - JT - inicio
             p_doc_transp-shtyp EQ c_z005 ).
*-CS2021001045 - 08.03.2022 - JT - fim

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum   = p_cte_avulso
        CHANGING
          p_parid_pc = wa_parceiros-parid.

      SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_parceiros-parid.
    ENDIF.

    p_identifica-cmunini = wa_lfa1-txjcd+3(7).
    p_identifica-ufini   = wa_lfa1-txjcd(3).

    SELECT SINGLE * INTO cidade
      FROM j_1btxjurt
     WHERE spras      = p_identifica-spras
       AND country    = p_identifica-country
       AND taxjurcode = wa_lfa1-txjcd.

    IF sy-subrc IS INITIAL.
      p_identifica-nmunini = cidade-text.
    ENDIF.

    IF ( p_doc_transp-abfer = c_1 OR p_doc_transp-abfer = c_3 ) AND ( p_doc_transp-shtyp NE c_z026 AND
*-CS2021001045 - 08.03.2022 - JT - inicio
         p_doc_transp-shtyp NE c_z005 ).
*-CS2021001045 - 08.03.2022 - JT - fim

      READ TABLE p_parceiros INTO wa_parceiros WITH KEY parvw  = c_lr.

      SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_parceiros-parid.

      p_identifica-cmunfim = wa_kna1-txjcd+3(7).
      p_identifica-uffim   = wa_kna1-txjcd(3).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = p_identifica-spras
         AND country    = p_identifica-country
         AND taxjurcode = wa_kna1-txjcd.

      IF sy-subrc IS INITIAL.
        p_identifica-nmunfim = cidade-text.
      ENDIF.

    ELSEIF ( p_doc_transp-abfer EQ c_1 ) AND ( p_doc_transp-shtyp EQ c_z026 OR
*-CS2021001045 - 08.03.2022 - JT - inicio
             p_doc_transp-shtyp EQ c_z005 ).
*-CS2021001045 - 08.03.2022 - JT - fim

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum   = p_cte_avulso
        CHANGING
          p_parid_lr = wa_parceiros-parid.

      SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_parceiros-parid.

      p_identifica-cmunfim = wa_kna1-txjcd+3(7).
      p_identifica-uffim   = wa_kna1-txjcd(3).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = p_identifica-spras
         AND country    = p_identifica-country
         AND taxjurcode = wa_kna1-txjcd.

      IF sy-subrc IS INITIAL.
        p_identifica-nmunfim = cidade-text.
      ENDIF.

    ELSEIF ( p_doc_transp-abfer EQ c_2 OR p_doc_transp-abfer = c_4 ) AND ( p_doc_transp-shtyp NE c_z021 ).

      READ TABLE p_t001w INTO wa_t001w INDEX 1.

      p_identifica-cmunfim = wa_t001w-txjcd+3(7).
      p_identifica-uffim   = wa_t001w-txjcd(3).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = p_identifica-spras
         AND country    = p_identifica-country
         AND taxjurcode = wa_t001w-txjcd.

      IF sy-subrc IS INITIAL.
        p_identifica-nmunfim = cidade-text.
      ENDIF.

    ELSEIF ( p_doc_transp-abfer EQ c_2 ) AND ( p_doc_transp-shtyp EQ c_z021 ).

      READ TABLE it_cte_info_nota INDEX 1.

      IF ( it_cte_info_nota[] IS INITIAL ) OR ( it_cte_info_nota-docnum_nf IS INITIAL ).
        SELECT SINGLE * FROM vttp INTO wa_vttp WHERE tknum EQ p_doc_transp-tknum.
        IF ( sy-subrc EQ 0 ).
          SELECT SINGLE * FROM vbpa INTO wa_vbpa WHERE vbeln EQ wa_vttp-vbeln
                                                   AND parvw EQ 'LR'.
          SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ wa_vbpa-kunnr.
        ENDIF.
        ""
        p_identifica-cmunfim = wa_kna1-txjcd+3(7).
        p_identifica-uffim   = wa_kna1-txjcd(3).

        SELECT SINGLE * INTO cidade
          FROM j_1btxjurt
         WHERE spras      = p_identifica-spras
           AND country    = p_identifica-country
           AND taxjurcode = wa_kna1-txjcd.

        IF sy-subrc IS INITIAL.
          p_identifica-nmunfim  = cidade-text.
        ENDIF.
        ""
      ELSE.

        "CS2017002156
        DATA(_define_cmunfim) = ''.

        CLEAR: wa_vttp, wa_vbpa, wa_kna1.
        SELECT SINGLE * FROM vttp INTO wa_vttp WHERE tknum EQ p_doc_transp-tknum.
        IF ( sy-subrc EQ 0 ) AND ( p_doc_transp-tknum IS NOT INITIAL ) AND ( wa_vttp-vbeln IS NOT INITIAL ) .
          SELECT SINGLE * FROM vbpa INTO wa_vbpa WHERE vbeln EQ wa_vttp-vbeln
                                                   AND parvw EQ 'LR'.

          IF ( sy-subrc = 0 ) AND ( wa_vbpa-kunnr IS NOT INITIAL ).
            SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ wa_vbpa-kunnr.

            p_identifica-cmunfim = wa_kna1-txjcd+3(7).
            p_identifica-uffim   = wa_kna1-txjcd(3).

            SELECT SINGLE * INTO cidade
              FROM j_1btxjurt
             WHERE spras      = p_identifica-spras
               AND country    = p_identifica-country
               AND taxjurcode = wa_kna1-txjcd.

            IF sy-subrc IS INITIAL.
              p_identifica-nmunfim  = cidade-text.
            ENDIF.

            _define_cmunfim = 'X'.
          ENDIF.
        ENDIF.
        "Fim CS2017002156

        READ TABLE it_cte_info_nota INDEX 1.

        IF ( sy-subrc IS INITIAL ) AND ( _define_cmunfim IS INITIAL ).

          SELECT SINGLE * INTO wa_j_1bnfdoc_nf
            FROM j_1bnfdoc
           WHERE docnum EQ it_cte_info_nota-docnum_nf.

          SELECT SINGLE * INTO wa_t001w
            FROM t001w
           WHERE werks EQ wa_j_1bnfdoc_nf-branch.

          p_identifica-cmunfim = wa_t001w-txjcd+3(7).
          p_identifica-uffim   = wa_t001w-txjcd(3).

          SELECT SINGLE * INTO cidade
            FROM j_1btxjurt
           WHERE spras      = p_identifica-spras
             AND country    = p_identifica-country
             AND taxjurcode = wa_t001w-txjcd.

          IF sy-subrc IS INITIAL.
            p_identifica-nmunfim = cidade-text.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

    IF it_cte_info_nota[] IS INITIAL.
      SELECT SINGLE * FROM vttp INTO wa_vttp WHERE tknum EQ p_doc_transp-tknum.
      IF ( sy-subrc EQ 0 ).
        SELECT SINGLE * FROM vbpa INTO wa_vbpa WHERE vbeln EQ wa_vttp-vbeln
                                                 AND parvw EQ 'PC'.
        SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_vbpa-lifnr.
      ENDIF.
      p_identifica-cmunenv = wa_lfa1-txjcd+3(7).
      p_identifica-ufenv   = wa_lfa1-txjcd(3).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = p_identifica-spras
         AND country    = p_identifica-country
         AND taxjurcode = wa_lfa1-txjcd.

      IF sy-subrc IS INITIAL.
        p_identifica-nmunenv = cidade-text.
      ENDIF.

    ELSE.
      CALL FUNCTION 'Z_SD_INFO_CIDADE_EMITE'
        EXPORTING
          wa_j_1bnfdoc = wa_j_1bnfdoc
        IMPORTING
          taxjurcode   = taxjurcode.

      IF NOT taxjurcode IS INITIAL.
        p_identifica-cmunenv = taxjurcode+3(7).
        p_identifica-ufenv   = taxjurcode(3).

        SELECT SINGLE * INTO cidade
          FROM j_1btxjurt
         WHERE spras      = p_identifica-spras
           AND country    = p_identifica-country
           AND taxjurcode = taxjurcode.

        IF sy-subrc IS INITIAL.
          p_identifica-nmunenv = cidade-text.
        ENDIF.

      ENDIF.
    ENDIF.

    CLEAR: wa_parceiros, vl_kunnr_ag.

    READ TABLE p_parceiros INTO wa_parceiros WITH KEY parvw  = c_ag.
    IF sy-subrc = 0.
      vl_kunnr_ag = wa_parceiros-parid.
    ENDIF.

    CLEAR: wa_parceiros, vl_kunnr_rg.

    READ TABLE p_parceiros INTO wa_parceiros WITH KEY parvw  = c_rg.
    IF sy-subrc = 0.
      vl_kunnr_rg = wa_parceiros-parid.
    ENDIF.

*Preencher com:
*0-Remetente;
*3-Destinatário;

    IF ( p_doc_transp-shtyp = c_z020 ) OR ( p_doc_transp-shtyp = c_z021 ).         "Transferência
      p_identifica-toma = c_3.
    ELSEIF p_doc_transp-shtyp = c_z018.     "Venda Triangular
      p_identifica-toma = c_0.
    ELSEIF p_doc_transp-shtyp = c_z009.     "Devolução de Venda
      p_identifica-toma = c_3.
    ELSEIF p_doc_transp-shtyp = c_z026 OR     "Frete Terceiro
*-CS2021001045 - 08.03.2022 - JT - inicio
           p_doc_transp-shtyp = c_z005.
*-CS2021001045 - 08.03.2022 - JT - fim

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum   = p_cte_avulso
        CHANGING
          p_parid    = p_parid
          p_parid_we = p_parid_we
          p_parid_rg = p_parid_rg.

      IF p_parid_rg EQ p_parid.
        p_identifica-toma   = c_0.
        p_identifica-forpag = c_0.
      ELSEIF p_parid_rg EQ p_parid_we.
        p_identifica-toma   = c_3.
        p_identifica-forpag = c_1.
      ELSE.
        p_identifica-toma   = c_4.
        p_identifica-forpag = c_0.
      ENDIF.

    ELSEIF vl_kunnr_ag = vl_kunnr_rg.
      p_identifica-toma = c_0.
    ELSE.
      p_identifica-toma = c_4.
    ENDIF.

    SELECT * INTO TABLE it_vfkp
      FROM vfkp
     WHERE rebel = p_doc_transp-tknum
       AND refty = c_8
       AND fkpty = c_z001.

    READ TABLE it_vfkp INTO wa_vfkp INDEX 1.

    p_identifica-waers      = wa_vfkp-waers.

* Ttipos de condição de frete
    IF it_vfkp[] IS NOT INITIAL.

      SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @it_vfkp WHERE knumv = @it_vfkp-knumv AND kappl = @c_f AND kschl IN ( @c_zicm , @c_zicc , @c_zipt ) AND kinak EQ @vg_kinak INTO CORRESPONDING FIELDS OF TABLE @it_konv .

      SORT ti_konv BY kschl.

    ENDIF.

*    loop at it_konv into wa_konv
*                    where kschl <> c_zicm.
*      add wa_konv-kawrt to vl_bicm.
*      if vl_aliq is initial.
*        vl_aliq = wa_konv-kbetr / 10.
*      endif.
*    endloop.

    LOOP AT it_konv INTO wa_konv.

      IF ( wa_konv-kschl EQ c_zicm ).
        CLEAR: vl_bicm.

        "vl_bicm = wa_konv-kbetr / 10.
        SELECT SINGLE * FROM j_1bnfstx INTO wa_j_1bnfstx WHERE docnum EQ p_cte_avulso.
        vl_bicm = wa_j_1bnfstx-base.

        IF ( vl_bicm > 0 ).
          vl_bicm = wa_j_1bnfstx-base.
*          IF ( VL_BICM NE 100 ).
*            CLEAR: VL_BICM.
*            VL_BICM = ( WA_J_1BNFLIN-NETWR * WA_KONV-KBETR ) / 1000.
*          ENDIF.
        ENDIF.

      ENDIF.

      IF vl_aliq IS INITIAL AND ( wa_konv-kschl NE c_zicm ).
        vl_aliq = wa_konv-kbetr / 10.
      ENDIF.
    ENDLOOP.


    IF vl_bicm IS NOT INITIAL.
      p_identifica-cst      = c_00.
      p_identifica-vbc      = vl_bicm.
    ELSE.
      p_identifica-cst      = c_40.
    ENDIF.

    IF vl_aliq IS NOT INITIAL.
      p_identifica-picms    = vl_aliq.
    ENDIF.

    LOOP AT it_vfkp INTO wa_vfkp.
      ADD wa_vfkp-kzwi6 TO vl_vicms.
    ENDLOOP.

    IF vl_vicms IS NOT INITIAL.
      MOVE vl_vicms TO p_identifica-vicms.
    ENDIF.

    IF wa_j_1bnfdoc-doctyp = '2'. "Complementar

      CLEAR: it_konv[], wa_j_1bnfstx.
      CLEAR: p_identifica-cst, p_identifica-vbc, p_identifica-picms, p_identifica-vicms, vl_bicm, vl_aliq, vl_vicms, vl_taxsit.

      SELECT SINGLE *
        FROM j_1batl1 INTO @DATA(wa_j_1batl1)
       WHERE taxlaw = @wa_j_1bnflin-taxlw1.

      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = wa_j_1batl1-taxsit
        IMPORTING
          output = vl_taxsit.

      CASE vl_taxsit.

        WHEN: '00'.
          SELECT SINGLE *
            FROM j_1bnfstx AS a INTO wa_j_1bnfstx
           WHERE a~docnum = p_cte_avulso
             AND EXISTS ( SELECT *
                            FROM j_1baj
                           WHERE taxtyp = a~taxtyp
                             AND taxgrp = 'ICMS' ).

          IF ( sy-subrc EQ 0 ).
            p_identifica-cst   = vl_taxsit.
            p_identifica-vbc   = wa_j_1bnfstx-base.
            p_identifica-picms = wa_j_1bnfstx-rate.
            p_identifica-vicms = wa_j_1bnfstx-taxval.
          ENDIF.

        WHEN: '40' OR '41' OR '51'.
          p_identifica-cst = vl_taxsit.
      ENDCASE.

    ELSE.
      "CS2017002892 - 15.01.2018
      SELECT SINGLE *
        FROM j_1batl1 INTO wa_j_1batl1
       WHERE taxlaw = wa_j_1bnflin-taxlw1.

      IF sy-subrc = 0.
        CLEAR: vl_taxsit.
        CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
          EXPORTING
            input  = wa_j_1batl1-taxsit
          IMPORTING
            output = vl_taxsit.

        CASE vl_taxsit.
          WHEN: '00'.
            SELECT SINGLE *
              FROM j_1bnfstx AS a INTO wa_j_1bnfstx
             WHERE a~docnum = p_cte_avulso
               AND EXISTS ( SELECT *
                              FROM j_1baj
                             WHERE taxtyp = a~taxtyp
                               AND taxgrp = 'ICMS' ).

            IF ( sy-subrc EQ 0 ).
              p_identifica-cst   = vl_taxsit.
            ENDIF.
          WHEN: '40' OR '41' OR '51'.
            p_identifica-cst = vl_taxsit.
        ENDCASE.
      ENDIF.
      " Fim CS2017002892 - 15.01.2018
    ENDIF.

    p_identifica-vtprest          = 0.
    p_identifica-vrec             = 0.
    p_identifica-predbc           = 0.
    p_identifica-vbcstret         = 0.
    p_identifica-vicmsstret       = 0.
    p_identifica-picmsstret       = 0.
    p_identifica-vcred            = 0.
    p_identifica-vlr_seguro       = 0.
    p_identifica-vlr_combustivel  = 0.
    p_identifica-vlr_pedagio      = 0.
    p_identifica-vlr_impostos     = 0.
    p_identifica-vlr_inss         = 0.
    p_identifica-vlr_iss          = 0.
    p_identifica-vlr_sest         = 0.
    p_identifica-vlr_irpf         = 0.
    p_identifica-vlr_iof          = 0.
    p_identifica-vlr_adiantamento = 0.
    p_identifica-vlr_pis_cofins   = 0.
    p_identifica-vlr_inss_lucro   = 0.

    CLEAR: it_konv[].
    FREE: it_prcd_elem.    "*-CS2023000780-20.09.2024-JT-#127476-inicio

    IF ( wa_j_1bnfdoc-doctyp EQ '2' ).
      p_identifica-vtprest = wa_j_1bnflin-netwr.
      p_identifica-vrec    = wa_j_1bnflin-netwr.
    ENDIF.

    IF ( it_vfkp[] IS NOT INITIAL ) AND ( wa_j_1bnfdoc-doctyp NE '2' ).

*---> 05/07/2022 - Migração S4 - DG
*      SELECT * INTO TABLE it_konv
*        FROM konv
*         FOR ALL ENTRIES IN it_vfkp
*       WHERE knumv = it_vfkp-knumv
*         AND kappl = c_f
*         AND kschl IN (c_zseg,c_zadm,c_zped,c_zset,z_zfre,z_zlot,c_zirf,c_ziof,c_zins,c_zadh,c_zhi1,c_zbh1,c_zsgh,c_zifh,c_zdeh,c_zvct,c_ziss)
*         AND kinak EQ vg_kinak.

      SELECT * INTO TABLE @DATA(it_konv_aux)
        FROM v_konv
         FOR ALL ENTRIES IN @it_vfkp
       WHERE knumv = @it_vfkp-knumv
         AND kappl = @c_f
         AND kschl IN (@c_zseg,@c_zadm,@c_zped,@c_zset,@z_zfre,@z_zlot,@c_zirf,@c_ziof,@c_zins,@c_zadh,@c_zhi1,@c_zbh1,@c_zsgh,@c_zifh,@c_zdeh,@c_zvct,@c_ziss)
         AND kinak EQ @vg_kinak.

      MOVE-CORRESPONDING it_konv_aux[] TO it_konv[].
*<--- 05/07/2022 - Migração S4 - DG

*-CS2023000780-20.09.2024-JT-#127476-inicio
      READ TABLE t_cte_trans INTO DATA(w_cte_trans) WITH KEY tp_veiculo = '0'.

      IF sy-subrc = 0.
        SELECT SINGLE indtyp
          INTO @DATA(_indtyp)
          FROM lfa1
         WHERE lifnr = @w_cte_trans-proprietario.

        IF sy-subrc = 0 AND _indtyp = 'Z3'.
          SELECT * INTO TABLE it_prcd_elem
            FROM prcd_elements
             FOR ALL ENTRIES IN it_vfkp
           WHERE knumv = it_vfkp-knumv
             AND kschl = c_zirf.
        ENDIF.
      ENDIF.
*-CS2023000780-20.09.2024-JT-#127476-fim

      LOOP AT it_konv INTO wa_komv WHERE kschl = z_zfre.
        ADD wa_komv-kwert TO p_identifica-vtprest.
        ADD wa_komv-kwert TO p_identifica-vrec.
        p_identifica-vlr_unit_frete = wa_komv-kbetr.
        p_identifica-unid_vlr_frete = wa_komv-kmein.
        CLEAR: p_identifica-frete_lotacao.
      ENDLOOP.

      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zvct.
        ADD wa_komv-kbetr TO p_identifica-vlr_triagem.
        ADD wa_komv-kbetr TO p_identifica-vrec.
      ENDLOOP.

      LOOP AT it_konv INTO wa_komv WHERE kschl = z_zlot.
        ADD wa_komv-kwert TO p_identifica-vtprest.
        ADD wa_komv-kwert TO p_identifica-vrec.
        p_identifica-frete_lotacao = c_x.
      ENDLOOP.

      "Somar Quando existir o ZADH
      LOOP AT it_konv INTO wa_komv WHERE kschl EQ c_zadh.
        ADD wa_komv-kwert TO p_identifica-vlr_pis_cofins.
        ADD wa_komv-kwert TO p_identifica-vrec.
      ENDLOOP.

      "Somar Quando existir o ZSGH
      LOOP AT it_konv INTO wa_komv WHERE kschl EQ c_zsgh.
        ADD wa_komv-kwert TO p_identifica-vrec.
      ENDLOOP.

      "Somar Quando existir o ZIFH
      LOOP AT it_konv INTO wa_komv WHERE kschl EQ c_zifh.
        ADD wa_komv-kwert TO p_identifica-vrec.
      ENDLOOP.

      "Somar Quando existir o ZDEH
      LOOP AT it_konv INTO wa_komv WHERE kschl EQ c_zdeh.
        ADD wa_komv-kwert TO p_identifica-vrec.
      ENDLOOP.

      "Somar Quando existir o ZHI1
      LOOP AT it_konv INTO wa_komv WHERE kschl EQ c_zhi1.
        ADD wa_komv-kwert TO p_identifica-vlr_inss_lucro.
        ADD wa_komv-kwert TO p_identifica-vrec.
      ENDLOOP.

      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zseg.
        ADD wa_komv-kwert TO p_identifica-vlr_seguro.
      ENDLOOP.

      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zadm.
        ADD wa_komv-kwert TO p_identifica-vlr_adiantamento.
      ENDLOOP.

      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zped.
        ADD wa_komv-kwert TO p_identifica-vlr_pedagio.
        p_identifica-rsp_pedagio = c_toma_ped5.
      ENDLOOP.

      "Impostos
      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zset.
        wa_komv-kwert = wa_komv-kwert * -1.
        ADD wa_komv-kwert TO p_identifica-vlr_impostos.
        ADD wa_komv-kwert TO p_identifica-vlr_sest.
      ENDLOOP.

*-CS2023000780-20.09.2024-JT-#127476-inicio
      IF it_prcd_elem[] IS NOT INITIAL.
        LOOP AT it_prcd_elem   INTO DATA(wa_prcd_elem) WHERE kschl = c_zirf.
          ADD wa_prcd_elem-kwert TO p_identifica-vlr_impostos.
          ADD wa_prcd_elem-kwert TO p_identifica-vlr_irpf.
        ENDLOOP.
      ELSE.
        LOOP AT it_konv        INTO wa_komv            WHERE kschl = c_zirf.
          wa_komv-kwert           = wa_komv-kwert * -1.
          ADD wa_komv-kwert      TO p_identifica-vlr_impostos.
          ADD wa_komv-kwert      TO p_identifica-vlr_irpf.
        ENDLOOP.
      ENDIF.
*-CS2023000780-20.09.2024-JT-#127476- fim

      LOOP AT it_konv INTO wa_komv WHERE kschl = c_ziof.
        ADD wa_komv-kwert TO p_identifica-vlr_impostos.
        ADD wa_komv-kwert TO p_identifica-vlr_iof.
      ENDLOOP.
      LOOP AT it_konv INTO wa_komv WHERE kschl = c_zins.
        wa_komv-kwert = wa_komv-kwert * -1.
        ADD wa_komv-kwert TO p_identifica-vlr_impostos.
        ADD wa_komv-kwert TO p_identifica-vlr_inss.
      ENDLOOP.
      LOOP AT it_konv INTO wa_komv WHERE kschl = c_ziss.
        ADD wa_komv-kwert TO p_identifica-vlr_impostos.
        ADD wa_komv-kwert TO p_identifica-vlr_iss.
      ENDLOOP.
    ENDIF.

    "Atualizado Mun.Ini e Fim J_1BNFDOC
    IF wa_j_1bnfdoc-docnum IS NOT INITIAL AND p_identifica-tknum IS NOT INITIAL. " Rubenilson Pereira - 15.09.25 #183774
*    IF wa_j_1bnfdoc-docnum IS NOT INITIAL.                                      " Rubenilson Pereira - 15.09.25 #183774

      CONCATENATE p_identifica-ufini p_identifica-cmunini INTO vl_cte_strt_lct SEPARATED BY space.
      CONCATENATE p_identifica-uffim p_identifica-cmunfim INTO vl_cte_end_lct  SEPARATED BY space.

      UPDATE j_1bnfdoc SET cte_strt_lct = vl_cte_strt_lct
                           cte_end_lct  = vl_cte_end_lct
       WHERE docnum EQ wa_j_1bnfdoc-docnum.

    ENDIF.

    CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
      EXPORTING
        p_docnum   = wa_j_1bnfdoc-docnum
      CHANGING
        p_bukrs    = wa_j_1bnfdoc-bukrs
        p_parid    = wa_j_1bnfdoc-parid
        p_tp_forne = p_identifica-tp_fornecimento
        p_dc_forne = p_identifica-dc_fornecimento.

  ENDIF.

ENDFUNCTION.
