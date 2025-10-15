
*======================================================================*
* PROJETO            : LES                                             *
* PROGRAMA           : ZLES0115                                        *
* TRANSACAO          : ZLES0151                                        *
* DESCRICAO          : Solicitação Aprovação Alteração Preço de Frete  *
*======================================================================*
* AUTOR              : Jean Antunes                                    *
* Solicitante        : Regina Santos                                   *
* DATA               : 13/12/2017                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*10.01.2017| JANTUNES | DEVK981410  |                                  *
*======================================================================*

REPORT zlesr0116 NO STANDARD PAGE HEADING.

TABLES: zlest0155, zsdt0001od, konh, zlest0185.

*======================================================================*
* TYPES
*======================================================================*
TYPES:
  BEGIN OF ty_ordem,
    id_ordem          TYPE zsdt0001od-id_ordem,
    nr_ordem          TYPE zsdt0001od-nr_ordem,
    dt_emissao        TYPE zsdt0001od-dt_emissao,
    dt_validade       TYPE zsdt0001od-dt_validade,
    nr_safra          TYPE zsdt0001od-nr_safra,
    id_bukrs          TYPE zsdt0001od-id_bukrs,
    id_branch         TYPE zsdt0001od-id_branch,
    id_bukrs_ag       TYPE zsdt0001od-id_bukrs_ag,
    id_branch_ag      TYPE zsdt0001od-id_branch_ag,
    id_local_coleta   TYPE zsdt0001od-id_local_coleta,
    id_local_destino  TYPE zsdt0001od-id_local_destino,
    id_local_descarga TYPE zsdt0001od-id_local_descarga,
    id_produto        TYPE zsdt0001od-id_produto,
    id_motorista      TYPE zsdt0001od-id_motorista,
    ds_placa_trator   TYPE zsdt0001od-ds_placa_trator,
    tp_status         TYPE zsdt0001od-tp_status,
    nr_peso_alvo      TYPE zsdt0001od-nr_peso_alvo,
    nr_frete_comb     TYPE zsdt0001od-nr_frete_comb,
  END OF ty_ordem,

  BEGIN OF ty_vbpa,   "PARCEIROS ORDEM DE VENDA"
    vbeln TYPE vbpa-vbeln,
    parvw TYPE vbpa-parvw,
    kunnr TYPE vbpa-kunnr,
    lifnr TYPE vbpa-lifnr,
  END OF ty_vbpa,

  BEGIN OF ty_lfa1,   "FORNECEDOR"
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
    lzone TYPE lfa1-lzone,
  END OF ty_lfa1,

  BEGIN OF ty_kna1,   "CLIENTE"
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
    lzone TYPE kna1-lzone,
  END OF ty_kna1,

  BEGIN OF ty_t001w,  "FILIAL"
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END OF ty_t001w,

  BEGIN OF ty_makt,   "MATERIAL"
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_zlest0185,   "viagem_ID
    id_ordem  TYPE zlest0185-id_ordem,
    viagem_id TYPE zlest0185-viagem_id,
    vbeln     TYPE zlest0185-vbeln,
    ebeln     TYPE zlest0185-ebeln,
  END OF ty_zlest0185,

  BEGIN OF ty_vbak,   "ORDEM DE VENDA - CABEÇALHO"
    vbeln    TYPE vbak-vbeln,
    auart    TYPE vbak-auart,
    bukrs_vf TYPE vbak-bukrs_vf,
  END OF ty_vbak,

  BEGIN OF ty_tvakt,   "ORDEM DE VENDA - TIPO"
    auart TYPE tvakt-auart,
    bezei TYPE tvakt-bezei,
    spras TYPE tvakt-spras,
  END OF ty_tvakt,

  BEGIN OF ty_vbap,   "ORDEM DE VANDA - ITEM"
    vbeln TYPE vbap-vbeln,
    route TYPE vbap-route,
    matnr TYPE vbap-matnr,
    werks TYPE vbap-werks,
  END OF ty_vbap,

  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
    reswk TYPE ekko-reswk,
    bsart TYPE ekko-bsart,
  END OF ty_ekko,

  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    matnr TYPE ekpo-matnr,
    werks TYPE ekpo-werks,
    inco1 TYPE ekpo-inco1,
  END OF ty_ekpo,

  BEGIN OF ty_ekpa,
    ebeln TYPE ekpa-ebeln,
    parvw TYPE ekpa-parvw,
    lifn2 TYPE ekpa-lifn2,
  END OF ty_ekpa,

  BEGIN OF ty_ekpv,
    ebeln TYPE ekpv-ebeln,
    route TYPE ekpv-route,
    kunnr TYPE ekpv-kunnr,
  END OF ty_ekpv,

  BEGIN OF ty_zsdt0011, "DEPARA TIPO ORDEM/TIPO ZSDT0001"
    tp_movimento TYPE zsdt0011-tp_movimento,
    auart        TYPE zsdt0011-auart,
    shtyp        TYPE zsdt0011-shtyp,
    bsart        TYPE zsdt0011-bsart,
  END OF ty_zsdt0011,

  BEGIN OF ty_konh,     "Condições (cabeçalho)
    kotabnr TYPE konh-kotabnr,
    kschl   TYPE konh-kschl,
*---> 10.07.2023 17:03:26 - Migração S4 - DL
*    vakey   TYPE konh-vakey,
    vakey   TYPE konh_kks-vakey,
*<--- 10.07.2023 17:03:26 - Migração S4 - DL
    knumh   TYPE konh-knumh,
  END OF ty_konh,

  BEGIN OF ty_konp,
    knumh TYPE konp-knumh,
    kbetr TYPE konp-kbetr,
  END OF ty_konp,

  BEGIN OF ty_zlest0071, "Valor de Frete TK11/TK12
    knumh TYPE zlest0071-knumh,
    kbetr TYPE zlest0071-kbetr,
  END OF ty_zlest0071,

  BEGIN OF ty_zlest0002,
    placa    TYPE zlest0002-pc_veiculo,
    agregado TYPE zlest0002-agregado,
  END OF ty_zlest0002,

  BEGIN OF ty_cadastro,
    filial           TYPE c LENGTH 45,
    nr_ordem         TYPE zsdt0001od-nr_ordem,
    dt_emissao       TYPE zlest0155-dt_emissao,
    dt_validade      TYPE zlest0155-dt_validade,
    tipo_ordem       TYPE c LENGTH 45,
    emissor_ordem    TYPE c LENGTH 60,
    filial_agente    TYPE zsdt0001od-id_bukrs_ag,
    produto          TYPE c LENGTH 80,
    ponto_coleta     TYPE c LENGTH 60,
    descarga         TYPE c LENGTH 60,
    destino          TYPE c LENGTH 60,
    motorista        TYPE c LENGTH 60,
    placa            TYPE zsdt0001od-ds_placa_trator,
    agregado         TYPE c LENGTH 03,
    peso_carga       TYPE zsdt0001od-nr_peso_alvo,
    preco_combinado  TYPE zsdt0001od-nr_frete_comb,
    valor_frete      TYPE konp-kbetr,
    valor_frete_neg  TYPE konp-kbetr,
    motivo_alteracao TYPE c LENGTH 150,
    bname            TYPE zlest0155-bname,
    dt_mod           TYPE zlest0155-dt_mod,
    hr_mod           TYPE zlest0155-hr_mod,
    status_aprov     TYPE zlest0155-status_aprov,
    ag_frete         TYPE zlest0155-ag_frete,
    nr_safra         TYPE zsdt0001od-nr_safra,
    viagem_id        TYPE zlest0185-viagem_id,
  END OF ty_cadastro,

  BEGIN OF ty_zlest0155,
    id_ordem          TYPE zlest0155-id_ordem,
    nr_ordem          TYPE zlest0155-nr_ordem,
    dt_emissao        TYPE zlest0155-dt_emissao,
    dt_validade       TYPE zlest0155-dt_validade,
    id_bukrs          TYPE zlest0155-id_bukrs,
    id_branch         TYPE zlest0155-id_branch,
    id_bukrs_ag       TYPE zlest0155-id_bukrs_ag,
    id_branch_ag      TYPE zlest0155-id_branch_ag,
    id_local_coleta   TYPE zlest0155-id_local_coleta,
    id_local_destino  TYPE zlest0155-id_local_destino,
    id_local_descarga TYPE zlest0155-id_local_descarga,
    id_produto        TYPE zlest0155-id_produto,
    id_motorista      TYPE zlest0155-id_motorista,
    ds_placa_trator   TYPE zlest0155-ds_placa_trator,
    nr_peso_alvo      TYPE zlest0155-nr_peso_alvo,
    kbetr             TYPE zlest0155-kbetr,
    vlr_frete_neg     TYPE zlest0155-vlr_frete_neg,
    motivo            TYPE zlest0155-motivo,
    bname             TYPE zlest0155-bname,
    dt_mod            TYPE zlest0155-dt_mod,
    hr_mod            TYPE zlest0155-hr_mod,
    status_aprov      TYPE zlest0155-status_aprov,
    vbeln             TYPE zlest0155-vbeln,
    vlr_frete_tk11    TYPE zlest0155-vlr_frete_tk11,
    ch_referencia     TYPE zlest0155-ch_referencia,
    ag_frete          TYPE zlest0155-ag_frete,
  END OF ty_zlest0155,

  BEGIN OF ty_od_busca,
    id_ordem        TYPE zsdt0001od-id_ordem,
    nr_ordem        TYPE zsdt0001od-nr_ordem,
    dt_emissao      TYPE zsdt0001od-dt_emissao,
    nr_safra        TYPE zsdt0001od-nr_safra,
    dt_validade     TYPE zsdt0001od-dt_validade,
    id_bukrs        TYPE zsdt0001od-id_bukrs,
    id_branch       TYPE zsdt0001od-id_branch,
    ds_placa_trator TYPE zsdt0001od-ds_placa_trator,
    id_local_coleta TYPE zsdt0001od-id_local_coleta,
    nr_frete_comb   TYPE zsdt0001od-nr_frete_comb,
    tp_status       TYPE zsdt0001od-tp_status,
  END OF ty_od_busca,

  BEGIN OF ty_saida,
    nr_ordem        TYPE zsdt0001od-nr_ordem,
    dt_emissao      TYPE zsdt0001od-dt_emissao,
    dt_validade     TYPE zsdt0001od-dt_validade,
    id_bukrs        TYPE zsdt0001od-id_bukrs,
    id_branch       TYPE zsdt0001od-id_branch,
    id_burks_ag     TYPE zsdt0001od-id_bukrs_ag,
    id_branch_ag    TYPE zsdt0001od-id_branch_ag,
    id_produto      TYPE zsdt0001od-id_produto,
    id_motorista    TYPE zsdt0001od-id_motorista,
    ds_placa_trator TYPE zsdt0001od-ds_placa_trator,
    tp_status       TYPE zsdt0001od-tp_status,
    nr_peso_alvo    TYPE zsdt0001od-nr_peso_alvo,
    nr_frete_comb   TYPE zsdt0001od-nr_frete_comb,
    vbeln           TYPE zlest0155-vbeln,
    vlr_frete_neg   TYPE zlest0155-vlr_frete_neg,
    motivo          TYPE zlest0155-motivo,
    status_aprov    TYPE zlest0155-status_aprov,
    dt_mod          TYPE zlest0155-dt_mod,
    hr_mod          TYPE zlest0155-hr_mod,
    bname           TYPE zlest0155-bname,
    vlr_frete_tk11  TYPE zlest0155-vlr_frete_tk11,
    ch_referencia   TYPE zlest0155-ch_referencia,
    ag_frete        TYPE zlest0155-ag_frete,
    kunnr           TYPE vbpa-kunnr,
    lifnr           TYPE vbpa-lifnr,
    f_name1         TYPE lfa1-name1,  "FORNECEDOR
    f_lzone         TYPE lfa1-lzone,
    c_name1         TYPE kna1-name1,  "CLIENTE
    c_lzone         TYPE kna1-lzone,
    fi_name1        TYPE t001w-name1, "FILIAL
    maktx           TYPE makt-maktx,
    auart           TYPE vbak-auart,
    bezei           TYPE tvakt-bezei,
    route           TYPE vbap-route,
    shtyp           TYPE zsdt0011-shtyp,
    nr_safra        TYPE zsdt0001od-nr_safra,
    viagem_id       TYPE zlest0185-viagem_id,
  END OF ty_saida,

  BEGIN OF ty_zsdt0001,
    vbeln        TYPE zsdt0001-vbeln,
    placa_cav    TYPE zsdt0001-placa_cav,
    motorista    TYPE zsdt0001-motorista,
    agente_frete TYPE zsdt0001-agente_frete,
    dt_movimento TYPE zsdt0001-dt_movimento,
    nro_nf_frete TYPE zsdt0001-nro_nf_frete,
  END OF ty_zsdt0001,

  BEGIN OF ty_ov_busca,
    vbeln TYPE vbak-vbeln,
    kunnr TYPE vbak-kunnr,
    erdat TYPE vbak-erdat,
  END OF ty_ov_busca.



*======================================================================*
* VARIAVEIS
*======================================================================*
DATA:
  xv1              TYPE zsdt0011-shtyp,  "Tp.Transp
  xv2              TYPE vbpa-lifnr,      "
  "XV3               TYPE LFA1-LZONE,
  xv3_aux          TYPE lfa1-lzone,
  "XV4               TYPE KNA1-LZONE,
  xv4_aux          TYPE kna1-lzone,
  xv6              TYPE zsdt0001od-id_produto,
  xv7              TYPE vbap-route,
  xv8(10)          TYPE c,
  xv9              TYPE zlest0185-viagem_id,
  xzp              TYPE lzonea,      " Zona de partida
  xzc              TYPE lzonez,      " Zona de chegada
  zfil             TYPE t001w-name1, "Filial
  xcclr            TYPE vbpa-lifnr, "VBPA-KUNNR,  "Código Forn Z1
  xlr              TYPE lfa1-name1,  "Nome Forn Z1
  xccag            TYPE vbpa-kunnr,  "Código Cliente AG
  xag              TYPE kna1-name1,  "Nome Cliente AG
  xcfpc            TYPE vbpa-lifnr,  "Código Fornecedor PC
  xdescid          TYPE kna1-kunnr,  "Local de Descarga
  xdescname        TYPE kna1-name1,  "Local de Descarga
  xpc              TYPE lfa1-name1,  "Nome Fornecedor PC
  xcfag            TYPE vbpa-lifnr,  "Código Fornecedor TR
  xfag             TYPE lfa1-name1,  "Nome Fornecedor
  xmot             TYPE lfa1-name1,  "Nome Motorista
  xpf1             TYPE zlest0071-kbetr, "Condições Seleção Preço de Frete
*---> 10.07.2023 17:04:00 - Migração S4 - DL
  xvsoma           TYPE konh_kks-vakey, " TYPE C,
*xvsoma           TYPE KONH-vakey, " TYPE C,
*<--- 10.07.2023 17:04:00 - Migração S4 - DL

  erro             TYPE c VALUE IS INITIAL,
  "NRORDEM          TYPE ZLEST0155-NR_ORDEM,
  lc_zona          TYPE c LENGTH 20,
  ag_frete_txt(30) TYPE c,
  ck_alterou_oc    TYPE c LENGTH 1,
  ck_alterou_ag    TYPE c LENGTH 1,
  ck_alterou_pd    TYPE c LENGTH 1.


TYPES: r_xpf1 TYPE RANGE OF konh-knumh.
TYPES: l_xpf1 TYPE LINE  OF r_xpf1.
TYPES: r_kna1 TYPE RANGE OF kna1-kunnr.
TYPES: l_kna1 TYPE LINE  OF r_kna1.

DATA: wa_xpf1  TYPE l_xpf1,
      it_xpf1  TYPE r_xpf1,
      it_kunnr TYPE r_kna1.

DATA:
      xv5 TYPE c LENGTH 10.

DATA:
  obj_alv          TYPE REF TO cl_gui_alv_grid,
  obj_cont         TYPE REF TO cl_gui_custom_container,
  it_fcat          TYPE lvc_t_fcat,
  str              TYPE REF TO data,
  data_modifica    TYPE c LENGTH 20,
  status           TYPE c LENGTH 20,
  usuario_modifica TYPE c LENGTH 30.

*======================================================================*
* TABELAS INTERNAS
*======================================================================*
DATA: it_vbpa       TYPE TABLE OF ty_vbpa,
      it_lfa1       TYPE TABLE OF ty_lfa1,
      it_lfa1_dest  TYPE TABLE OF ty_lfa1,
      it_ordem      TYPE TABLE OF ty_ordem,
      it_od_busca   TYPE TABLE OF ty_od_busca,
      it_ov_busca   TYPE TABLE OF ty_ov_busca,
      it_kna1       TYPE TABLE OF ty_kna1,
      it_t001w      TYPE TABLE OF ty_t001w,
      it_makt       TYPE TABLE OF ty_makt,
      it_zlest0185  TYPE TABLE OF ty_zlest0185,
      it_vbak       TYPE TABLE OF ty_vbak,
      it_ekko       TYPE TABLE OF ty_ekko,
      it_ekpo       TYPE TABLE OF ty_ekpo,
      it_ekpa       TYPE TABLE OF ty_ekpa,
      it_ekpv       TYPE TABLE OF ty_ekpv,
      it_tvakt      TYPE TABLE OF ty_tvakt,
      it_vbap       TYPE TABLE OF ty_vbap,
      it_zlest0002  TYPE TABLE OF ty_zlest0002,
      it_zsdt0011   TYPE TABLE OF ty_zsdt0011,
      it_zsdt0001od TYPE TABLE OF ty_ordem,
      it_konh       TYPE TABLE OF ty_konh,
      it_konp       TYPE TABLE OF ty_konp,
      it_konp2      TYPE TABLE OF ty_konp,
      it_zlest0155  TYPE TABLE OF zlest0155,
      it_cadastro   TYPE TABLE OF ty_cadastro,
      it_zlest0071  TYPE TABLE OF ty_zlest0071,
      it_ty_saida   TYPE TABLE OF ty_saida,
      it_zsdt0001   TYPE TABLE OF ty_zsdt0001,
      it_return     TYPE TABLE OF ddshretval,
      it_vbak_aux   TYPE TABLE OF vbak.

**======================================================================*
** DATA * WORKAREA
**======================================================================*
DATA: wa_zsdt0001od LIKE LINE OF it_zsdt0001od,
      wa_vbpa       LIKE LINE OF it_vbpa,
      wa_lfa1       LIKE LINE OF it_lfa1,
      wa_lfa1_dest  LIKE LINE OF it_lfa1,
      wa_kna1       LIKE LINE OF it_kna1,
      wa_t001w      LIKE LINE OF it_t001w,
      wa_makt       LIKE LINE OF it_makt,
      wa_zlest0185  LIKE LINE OF it_zlest0185,
      wa_vbak       LIKE LINE OF it_vbak,
      wa_tvakt      LIKE LINE OF it_tvakt,
      wa_vbap       LIKE LINE OF it_vbap,
      wa_ekko       LIKE LINE OF it_ekko,
      wa_ekpo       LIKE LINE OF it_ekpo,
      wa_ekpa       LIKE LINE OF it_ekpa,
      wa_ekpv       LIKE LINE OF it_ekpv,
      wa_zlest0002  LIKE LINE OF it_zlest0002,
      wa_zsdt0011   LIKE LINE OF it_zsdt0011,
      wa_konh       LIKE LINE OF it_konh,
      wa_konp       LIKE LINE OF it_konp,
      wa_konp2      LIKE LINE OF it_konp,
      wa_cadastro   LIKE LINE OF it_cadastro,
      wa_zlest0071  LIKE LINE OF it_zlest0071,
      wa_ty_saida   TYPE ty_saida,
      wa_zlest0155  TYPE zlest0155,
      wa_zsdt0001   LIKE LINE OF it_zsdt0001,
      wa_return     LIKE LINE OF it_return.

**======================================================================*
** PARAMETERS
**======================================================================*
DATA: vfrete      TYPE zlest0155-vlr_frete_neg,
      motivo      TYPE c LENGTH 150,
      od_venda    TYPE vbak-vbeln,
      gb_id_ordem TYPE zsdt0001od-id_ordem.

START-OF-SELECTION.
  CALL SCREEN 0100.

*======================================================================*
* FORM F_BUSCA_ORDEM
*======================================================================*
FORM f_busca_ordem.

  PERFORM f_limpa_wa.

  SELECT SINGLE nr_ordem
    FROM zlest0155
    INTO @DATA(wl_zlest0155)
    WHERE id_ordem = @gb_id_ordem.

  IF wl_zlest0155 IS INITIAL.

    PERFORM f_seleciona_dados.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
          screen-input = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ELSE.

    SELECT vbeln
           vlr_frete_neg
           motivo
           status_aprov
           dt_mod
           hr_mod
           bname
           vlr_frete_tk11
           ch_referencia
           ag_frete
      FROM zlest0155
      INTO CORRESPONDING FIELDS OF TABLE it_ty_saida
    WHERE id_ordem = gb_id_ordem.

    PERFORM f_seleciona_dados.

  ENDIF.

ENDFORM.

*======================================================================*
* FORM F_SELECIONA_DADOS
*======================================================================*

FORM f_seleciona_dados.

  IF wa_zsdt0001od IS INITIAL.

    SELECT SINGLE id_ordem              "Seleção Ordem de Carregamento
                  nr_ordem
                  dt_emissao
                  dt_validade
                  nr_safra
                  id_bukrs
                  id_branch
                  id_bukrs_ag
                  id_branch_ag
                  id_local_coleta
                  id_local_destino
                  id_local_descarga
                  id_produto
                  id_motorista
                  ds_placa_trator
                  tp_status
                  nr_peso_alvo
                  nr_frete_comb
      FROM zsdt0001od
      INTO wa_zsdt0001od
      WHERE id_ordem = gb_id_ordem.

    IF wa_zsdt0001od IS NOT INITIAL.

      APPEND wa_zsdt0001od TO it_zsdt0001od.

      SELECT lifnr               "Seleção Nome do Fornecedor / Nome do Motorista
             name1
             lzone
        FROM lfa1
        INTO TABLE it_lfa1
      WHERE lifnr EQ wa_zsdt0001od-id_local_coleta OR
            lifnr EQ wa_zsdt0001od-id_motorista.

      SELECT lifnr name1 lzone             "Seleção Nome do Cliente
        FROM lfa1
        INTO TABLE it_lfa1_dest
        WHERE lifnr = wa_zsdt0001od-id_local_destino.

      SELECT kunnr name1 lzone             "Seleção Nome do Cliente / Descarga
       FROM kna1
       APPENDING TABLE it_kna1
       WHERE kunnr = wa_zsdt0001od-id_local_descarga.

      SELECT werks name1              "Seleção Nome da Filial / Nome da Filial AG
        FROM t001w
        INTO TABLE it_t001w
        WHERE werks = wa_zsdt0001od-id_branch OR
              werks = wa_zsdt0001od-id_branch_ag.

      SELECT matnr spras maktx             "Seleção Nome do Material
        FROM makt
        INTO TABLE it_makt
        WHERE matnr = wa_zsdt0001od-id_produto
        AND   spras = 'PT'.


      SELECT id_ordem     "Seleção para viagem_ID
             viagem_id
             vbeln
             ebeln
        FROM zlest0185
        INTO TABLE it_zlest0185
        WHERE id_ordem = wa_zsdt0001od-id_ordem.

    ENDIF.

    CLEAR wa_zsdt0001od.

  ENDIF.
ENDFORM.

*======================================================================*
* FORM F_VALIDACAO_CAMPOS
*======================================================================*
FORM f_validacao_campos.

  IF ( wa_zsdt0001od-dt_emissao < sy-datum ).
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem de Carregamento Vencida!'.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF ( wa_zsdt0001od-dt_validade > sy-datum ).
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem de Carregamento Vencida!'.
    LEAVE TO SCREEN 0.
  ENDIF.

  IF ( wa_zsdt0001od-tp_status <> 'AB' ).
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem de Carregamento Não está com Status de Aberto'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

*======================================================================*
* FORM F_AGRUPA_DADOS                                                  *
*======================================================================*
FORM f_agrupa_dados.

  LOOP AT it_ty_saida INTO wa_ty_saida.

    od_venda                 = wa_ty_saida-vbeln.
    xpf1                     = wa_ty_saida-vlr_frete_tk11.
    vfrete                   = wa_ty_saida-vlr_frete_neg.
    motivo                   = wa_ty_saida-motivo.
    wa_cadastro-status_aprov = wa_ty_saida-status_aprov.
    data_modifica            = |{ wa_ty_saida-dt_mod }{ wa_ty_saida-hr_mod }|.
    usuario_modifica         = wa_ty_saida-bname.
    xv2                      = wa_ty_saida-ag_frete.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = xv2
      IMPORTING
        output = xv2.

    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wl_ag)
      WHERE lifnr EQ @xv2.

    IF wl_ag IS NOT INITIAL.
      ag_frete_txt = wl_ag-name1.
    ELSE.
      CLEAR xv2.
      MESSAGE 'Agente de Frete não encontrado!' TYPE 'I'.
    ENDIF.

    CASE wa_ty_saida-status_aprov.
      WHEN 1.
        status = 'Aprovado'.
      WHEN 2.
        status = 'Reprovado'.
      WHEN 3.
        status = 'Bloqueado'.
      WHEN 9.
        status = 'Aguardando Aprovação'.
    ENDCASE.

  ENDLOOP.

  LOOP AT it_zsdt0001od INTO  wa_zsdt0001od.

    wa_ty_saida-nr_ordem =        wa_zsdt0001od-nr_ordem.
    wa_ty_saida-dt_emissao =      wa_zsdt0001od-dt_emissao.
    wa_ty_saida-dt_validade =     wa_zsdt0001od-dt_validade.
    wa_ty_saida-id_bukrs =        wa_zsdt0001od-id_bukrs.
    wa_ty_saida-id_branch =       wa_zsdt0001od-id_branch.
    wa_ty_saida-id_burks_ag =     wa_zsdt0001od-id_bukrs_ag.
    wa_ty_saida-id_branch_ag =    wa_zsdt0001od-id_branch_ag.
    wa_ty_saida-id_produto =      wa_zsdt0001od-id_produto.
    wa_ty_saida-id_motorista =    wa_zsdt0001od-id_motorista.
    wa_ty_saida-ds_placa_trator = wa_zsdt0001od-ds_placa_trator.
    wa_ty_saida-tp_status =       wa_zsdt0001od-tp_status.
    wa_ty_saida-nr_peso_alvo =    wa_zsdt0001od-nr_peso_alvo.
    wa_ty_saida-nr_frete_comb =   wa_zsdt0001od-nr_frete_comb.

    xv6 =                         wa_zsdt0001od-id_produto.
    xcfag =                       wa_zsdt0001od-id_bukrs_ag.

    wa_ty_saida-nr_safra        =    wa_zsdt0001od-nr_safra.



    LOOP AT it_lfa1 INTO wa_lfa1.     "PESQUISA MOTORISTA

      IF wa_lfa1-lifnr = wa_zsdt0001od-id_motorista.
        xmot  = wa_lfa1-name1.
      ENDIF.

      IF wa_lfa1-lzone IS NOT INITIAL. "PONTO DE COLETA
        xcfpc = wa_zsdt0001od-id_local_coleta.
        xpc   = wa_lfa1-name1.
        "XV3   = WA_LFA1-LZONE.
      ENDIF.
    ENDLOOP.

    it_kunnr = VALUE #( FOR ls IN it_kna1       "CLIENTE
  ( sign   = 'I'
    option = 'EQ'
    low = ls-kunnr ) ).

    IF wa_vbpa-kunnr IN it_kunnr.
      wa_ty_saida-c_name1 = wa_kna1-name1.
      wa_ty_saida-c_lzone = wa_kna1-lzone.
      "XV4 = WA_KNA1-LZONE.
    ENDIF.

    "DESTINO:
    READ TABLE it_lfa1_dest INTO wa_lfa1_dest WITH KEY lifnr = wa_zsdt0001od-id_local_destino.
    xcclr = wa_zsdt0001od-id_local_destino.
    xlr   = wa_lfa1_dest-name1.


    "DESCARGA:
    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zsdt0001od-id_local_descarga.
    xdescid = wa_zsdt0001od-id_local_descarga.
    xdescname = wa_kna1-name1.


    "EMISSOR
    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0001od-id_branch_ag.
    xccag = wa_zsdt0001od-id_branch_ag.
    xag   = wa_t001w-name1.
    CLEAR wa_t001w.

  ENDLOOP.

  READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr.
  wa_ty_saida-c_name1 = wa_kna1-name1.
  wa_ty_saida-c_lzone = wa_kna1-lzone.
  "XV4 = WA_KNA1-LZONE.

  READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0001od-id_branch.
  wa_ty_saida-fi_name1 = wa_t001w-name1.
  zfil = wa_t001w-name1.

  READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0001od-id_produto.
  wa_ty_saida-maktx = wa_makt-maktx.

  READ TABLE it_zlest0185 INTO wa_zlest0185 WITH KEY id_ordem  = wa_zsdt0001od-id_ordem.
  wa_ty_saida-viagem_id = wa_zlest0185-viagem_id.

  SELECT *  "Busca nr. OV e agente de frete quando for formação de lote
  FROM   vbak
  INTO TABLE it_vbak_aux
  WHERE vbeln = wa_zlest0185-vbeln
    AND auart IN ('ZRFL', 'ZRDC').

  IF it_vbak_aux IS NOT INITIAL.
    SELECT SINGLE lifnr
      FROM vbpa
      INTO wa_ty_saida-ag_frete
    WHERE vbeln = wa_zlest0185-vbeln
      AND parvw = 'SP'.
  ENDIF.

  IF wa_zlest0185-vbeln IS NOT INITIAL.
    od_venda =  wa_zlest0185-vbeln.
  ELSE.
    od_venda =  wa_zlest0185-ebeln.
  ENDIF.


  MODIFY it_ty_saida INDEX 1 FROM wa_ty_saida.

ENDFORM.

*======================================================================*
* FORM F_VALIDA_CONDICOES
*======================================================================*

FORM f_valida_condicoes. "SELEÇÃO PREÇO DE FRETE

  CLEAR xpf1.

*  IF XV2 IS INITIAL.
  PERFORM f_seleciona_dados_ov.
*  ENDIF.

  IF xv2 IS INITIAL.
*      MESSAGE 'Informar Agente de Frete!' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR xvsoma.

  IF xpf1 IS INITIAL.
*    xvsoma = |{ xv1 }{ xv8 }{ xv9 }|.      "Tp.Transporte/contrato/viagem_id
    PERFORM f_select_valorfrete USING 'ZFRE' xv1 xv2 xv5 xv6 xv7 xv8 xv9 xzp xzc.
  ENDIF.


  IF xpf1 IS INITIAL.

    CLEAR sy-ucomm.
*    MESSAGE TEXT-019 TYPE 'I'.
*    EXIT.

  ENDIF.

ENDFORM.

*======================================================================*
* FORM F_FORMA_CADASTRO
*======================================================================*
FORM f_forma_cadastro.

  READ TABLE it_ty_saida
       INTO wa_ty_saida
       WITH KEY nr_ordem = wa_ty_saida-nr_ordem.

  wa_cadastro-nr_ordem         = wa_ty_saida-nr_ordem.
  wa_cadastro-dt_emissao       = wa_ty_saida-dt_emissao.
  wa_cadastro-dt_validade      = wa_ty_saida-dt_validade.
  wa_cadastro-filial           = |{ wa_ty_saida-id_branch }-{ zfil }|.
  wa_cadastro-emissor_ordem    = |{ xccag } - { xag }|.
  wa_cadastro-produto          = |{ wa_ty_saida-id_produto }-{ wa_ty_saida-maktx }|. "#EC CI_FLDEXT_OK[2215424] "*---> 19/07/2023 - Migração S4 - LO --> produto de tamanho 80
  wa_cadastro-ponto_coleta     = |{ xcfpc }-{ xpc }|.
  wa_cadastro-descarga         = |{ xdescid }-{ xdescname }|.
  wa_cadastro-destino          = |{ xcclr }-{ xlr }|.
  wa_cadastro-motorista        = |{ wa_ty_saida-id_motorista }-{ xmot }|.
  wa_cadastro-placa            = wa_ty_saida-ds_placa_trator.
  wa_cadastro-peso_carga       = wa_ty_saida-nr_peso_alvo.
  wa_cadastro-preco_combinado  = wa_zsdt0001od-nr_frete_comb.
  wa_cadastro-valor_frete      = xpf1."WA_TY_SAIDA-NR_FRETE_COMB.

  wa_cadastro-nr_safra         = wa_ty_saida-nr_safra.
  wa_cadastro-viagem_id        = wa_ty_saida-viagem_id.

  SHIFT wa_cadastro-emissor_ordem LEFT DELETING LEADING '0'.
  SHIFT wa_cadastro-produto       LEFT DELETING LEADING '0'.
  SHIFT wa_cadastro-ponto_coleta  LEFT DELETING LEADING '0'.
  SHIFT wa_cadastro-destino       LEFT DELETING LEADING '0'.
  SHIFT wa_cadastro-motorista     LEFT DELETING LEADING '0'.
  SHIFT wa_cadastro-descarga      LEFT DELETING LEADING '0'.

  "BUSCA INFORMAÇÃO PLACA
  SELECT SINGLE agregado
    FROM zlest0002
    INTO @DATA(vl_agregado)
    WHERE pc_veiculo EQ @wa_cadastro-placa.

  IF ( sy-subrc EQ 0 ).
    IF ( vl_agregado EQ '1').
      wa_cadastro-agregado = 'SIM'.
    ELSE.
      wa_cadastro-agregado = 'NAO'.
    ENDIF.
  ENDIF.

  APPEND wa_cadastro TO it_cadastro.

  LOOP AT it_cadastro INTO wa_cadastro.

    wa_zlest0155-id_ordem          = wa_zsdt0001od-id_ordem.
    wa_zlest0155-nr_ordem          = wa_zsdt0001od-nr_ordem.
    wa_zlest0155-dt_emissao        = wa_zsdt0001od-dt_emissao.
    wa_zlest0155-dt_validade       = wa_zsdt0001od-dt_validade.
    wa_zlest0155-vbeln             = od_venda.
    wa_zlest0155-id_bukrs          = wa_zsdt0001od-id_bukrs.
    wa_zlest0155-id_branch         = wa_zsdt0001od-id_branch.
    wa_zlest0155-id_bukrs_ag       = wa_zsdt0001od-id_bukrs_ag.
    wa_zlest0155-id_branch_ag      = wa_zsdt0001od-id_branch_ag.
    wa_zlest0155-id_local_coleta   = xcfpc.
    wa_zlest0155-id_local_destino  = xcclr.
    wa_zlest0155-id_local_descarga = wa_zsdt0001od-id_local_descarga.
    wa_zlest0155-id_produto        = wa_zsdt0001od-id_produto.
    wa_zlest0155-id_motorista      = wa_zsdt0001od-id_motorista.
    wa_zlest0155-ds_placa_trator   = wa_zsdt0001od-ds_placa_trator.
    wa_zlest0155-nr_peso_alvo      = wa_zsdt0001od-nr_peso_alvo.
    wa_zlest0155-kbetr             = wa_zsdt0001od-nr_frete_comb.
    wa_zlest0155-bname             = sy-uname.
    wa_zlest0155-dt_mod            = sy-datum.
    wa_zlest0155-hr_mod            = sy-uzeit.
    wa_zlest0155-vlr_frete_tk11    = xpf1.
    wa_zlest0155-ag_frete          = xv2.
    wa_zlest0155-viagem_id         = wa_cadastro-viagem_id.

  ENDLOOP.
ENDFORM.


*======================================================================*
* MODULE STATUS_0100  OUTPUT
*======================================================================*
MODULE status_0100 OUTPUT.
  CASE sy-ucomm.
    WHEN 'CRIAR' OR 'BTN_VBELN'.
      SET PF-STATUS 'STATUSSALVAR'.

    WHEN 'BUSCAR'.

      IF wa_cadastro-filial IS NOT INITIAL AND wa_cadastro-valor_frete_neg IS NOT INITIAL.
        SET PF-STATUS 'STATUSCHANGE'.
      ENDIF.

      IF motivo IS INITIAL.
        SET PF-STATUS 'STATUSSALVAR'.
      ENDIF.

      IF od_venda IS NOT INITIAL AND motivo IS NOT INITIAL AND vfrete IS NOT INITIAL.
        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'VFRETE' OR 'MOTIVO'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
        SET PF-STATUS 'STATUSSALVAR'.

      ENDIF.

*    WHEN 'SALVAR'.
*    WHEN 'NEW'.
    WHEN 'CHANGE'.
      SET PF-STATUS 'STATUSSALVAR'.
    WHEN OTHERS.
      IF erro = 1.
        SET PF-STATUS 'STATUSSALVAR'.
      ENDIF.
      SET PF-STATUS 'STATUSPADRAO'.
  ENDCASE.

  SET TITLEBAR 'SOLFRETE'.

ENDMODULE.


FORM f_valida_ordem_venda.

  IF xv2 IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = xv2
      IMPORTING
        output = xv2.

    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wl_lfa1)
      WHERE lifnr EQ @xv2.

    IF wl_lfa1 IS NOT INITIAL.
      ag_frete_txt = wl_lfa1-name1.
    ELSE.
      CLEAR xv2.
      erro = 1.
      MESSAGE 'Agente de Frete não encontrado!' TYPE 'I'.
    ENDIF.


    IF it_ekko IS NOT INITIAL.
      PERFORM f_valida_condicoes_pt.
    ELSE.
      IF xpf1 IS INITIAL.
*        xvsoma = |{ xv1 }{ xv8 }{ xv9 }|.      "Tp.Transporte/contrato/viagem_id
        PERFORM f_select_valorfrete USING  'ZFRE' xv1 xv2 xv5 xv6 xv7  xv8 xv9 xzp xzc.
      ENDIF.

      IF xpf1 IS INITIAL.
        CLEAR sy-ucomm.
        erro = 1.
*        MESSAGE TEXT-019 TYPE 'I'.
*        EXIT.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*======================================================================*
* MODULE USER_COMMAND_0100 INPUT
*======================================================================*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.

*    WHEN 'CREATE'.
*    WHEN 'BUSCAR'.

    WHEN 'SALVAR'.

      erro = 0.

      PERFORM f_valida_ov.

      PERFORM f_valida_ordem_venda.

      IF erro = 0.

        PERFORM f_grava_dados.
        CLEAR it_zlest0155.
        PERFORM f_limpa_wa.
        CLEAR: sy-ucomm, gb_id_ordem.

      ELSEIF erro = 1.

        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.

      ENDIF.
      EXIT.
    WHEN 'NEW'.
      PERFORM f_grava_dados.

  ENDCASE.
ENDMODULE.

*======================================================================*
* MODULE USER_COMMAND_0102 INPUT
*======================================================================*
MODULE user_command_0102 INPUT.
  CASE sy-ucomm.

    WHEN 'BUSCAR'.

      IF ck_alterou_oc EQ abap_true.

        DATA(nroc) = zsdt0001od-nr_ordem.

        SELECT id_ordem
               nr_ordem
               dt_emissao
               nr_safra
               dt_validade
               id_bukrs
               id_branch
               ds_placa_trator
               id_local_coleta
               nr_frete_comb
               tp_status
          FROM zsdt0001od
          INTO TABLE it_od_busca
         WHERE tp_status EQ 'AB'
           AND nr_ordem  EQ nroc
        ORDER BY dt_emissao DESCENDING.

        "AJUDA (F4)
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ID_ORDEM'
            value_org       = 'S'
          TABLES
            value_tab       = it_od_busca
            return_tab      = it_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        READ TABLE it_return INTO wa_return INDEX 1.
        IF sy-subrc IS INITIAL.
          gb_id_ordem = wa_return-fieldval.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gb_id_ordem
            IMPORTING
              output = gb_id_ordem.

          READ TABLE it_od_busca INTO DATA(wa_od_busca) WITH KEY id_ordem = gb_id_ordem.
          zsdt0001od-nr_ordem = wa_od_busca-nr_ordem.

          "IF PIDORDEM NE GB_ID_ORDEM.
          CLEAR: it_zlest0155, xv2, ag_frete_txt, xpf1.
          PERFORM f_limpa_wa.
          PERFORM f_executa_selecao.
          "ENDIF.

          IF od_venda IS NOT INITIAL.
            PERFORM f_valida_ov.
          ENDIF.
        ENDIF.

        CLEAR: ck_alterou_oc.

      ENDIF.

    WHEN 'SALVAR'.

  ENDCASE.
ENDMODULE.



*======================================================================*
* FORM F_GRAVA_DADOS
*======================================================================*

FORM f_grava_dados.

  CHECK sy-subrc = 0.

  IF vfrete IS INITIAL OR motivo IS INITIAL OR od_venda IS INITIAL.
    MESSAGE TEXT-011 TYPE 'S'.
    EXIT.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = od_venda
      IMPORTING
        output = od_venda.
  ENDIF.

  IF zlest0155-status_aprov = 'A'.
    MESSAGE TEXT-014 TYPE 'E'.
  ELSE.

    TRY.
        wa_zlest0155-vlr_frete_neg  = vfrete.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.

    TRY .
        zcl_ordem_carregamento=>get_instance(
          )->set_ordem( i_id_ordem = wa_zlest0155-id_ordem
          )->get_ck_viagem_autorizada(
          )->get_ck_viagem_nao_carregada(
          )->set_autorizar_preco_carguero( i_preco = wa_zlest0155-vlr_frete_neg
          ).
      CATCH zcx_ordem_carregamento INTO DATA(ex_ordem).
        ex_ordem->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        EXIT.
    ENDTRY.

*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          "Verificar se Possui Viagem Carguero e Enviar Aletração de Preço""""""""""""""""""""""""""
*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          TRY .
*              ZCL_ORDEM_CARREGAMENTO=>GET_INSTANCE(
*                )->SET_ORDEM( I_ID_ORDEM = WL_ESTRA-ID_ORDEM
*                )->SET_AUTORIZAR_PRECO_CARGUERO( I_PRECO = WA_ZLEST0155-VLR_FRETE_NEG
*                ).
*            CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORDEM).
*              EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*              MSG = ZCL_STRING=>CONCAT( S1 = 'CARGUERO:' S2 = MTEXT SP = SPACE ).
*              EXIT.
*          ENDTRY.
*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    wa_zlest0155-ag_frete       = xv2+6(4).
    wa_zlest0155-motivo         = motivo.
    wa_zlest0155-vbeln          = od_venda.
    wa_zlest0155-status_aprov   = 9.
    wa_zlest0155-vlr_frete_tk11 = xpf1.

    MODIFY zlest0155 FROM wa_zlest0155.
    MESSAGE TEXT-010 TYPE 'S'.

  ENDIF.

  PERFORM f_limpa_wa.
  CLEAR: xv2, ag_frete_txt, gb_id_ordem.

ENDFORM.

FORM f_limpa_wa.

  CLEAR:  wa_zsdt0001od, wa_vbpa, wa_lfa1, wa_kna1, wa_t001w, wa_makt, wa_vbak,
          wa_tvakt, wa_vbap, wa_zsdt0011, wa_konh, wa_cadastro, it_return, wa_zlest0071,
          wa_ty_saida, vfrete, motivo, status, data_modifica, usuario_modifica, od_venda, xpf1.

  FREE: it_vbpa, it_lfa1, it_ordem, it_kna1, it_t001w, it_makt, it_vbak, it_tvakt, it_vbap,
        it_zsdt0011, it_zsdt0001od, it_konh, it_zlest0155, it_cadastro, it_zlest0071, it_return, it_ty_saida.

ENDFORM.

*======================================================================*
* FORM F_SELECT_VALORFRETE
*======================================================================*

FORM f_select_valorfrete USING VALUE(p_1477)
                               xv1
                               xv2
                               xv5
                               xv6
                               xv7
                               xv8
                               xv9
                               xzp
                               xzc.


  FREE: it_konh[], it_konp2[], it_konp[].
  CLEAR: wa_konh, wa_konp, wa_konp2.
  IF xv8 IS NOT INITIAL.
    xv5 = xv8.
  ENDIF.

  " DEVK9A1Q47 - 20.10.2023 LES - Selecionar Valor frete na zcl_calc_frete=>get_valor_frete
  TRY .
      zcl_calc_frete=>get_valor_frete(
        EXPORTING
          i_kappl           = 'F'                " Aplicação
          i_kschl           = p_1477             " Tipo de condição
          i_shtyp           = xv1                " Tipo de transporte
          i_tdlnr           = xv2                " Nº do agente de frete
          i_add01           = xv5                " agregado
          i_matnr           = xv6                " material
          i_route           = xv7                " Itinerário
          i_viagem_id       = xv9                " id viagem
          i_lzonea          = xzp                " Zona de partida
          i_lzonez          = xzc                " Zona de chegada
        IMPORTING
          e_kbetr           = DATA(e_kbetr) ).   " Montante/porcentagem de condição no caso de não haver escala
    CATCH zcx_calc_frete INTO DATA(ex_calc_frete).
      clear e_kbetr.
      ex_calc_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
  ENDTRY.

  xpf1 = e_kbetr.


ENDFORM.

*======================================================================*
* FORM F_EXECUTA_SELEÇÃO
*======================================================================*
FORM f_executa_selecao .

  PERFORM: f_busca_ordem,
           f_agrupa_dados,
           f_forma_cadastro.

ENDFORM.

MODULE exit_screen INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OTHERS'.
  ENDCASE.
ENDMODULE.

*======================================================================*
* FORM F_VALIDA_OV
*======================================================================*
FORM f_valida_ov .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = od_venda
    IMPORTING
      output = od_venda.

  SELECT vbeln
   FROM vbak
   INTO TABLE it_vbak
  WHERE vbeln = od_venda.

  READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = od_venda.

  IF it_vbak[] IS INITIAL.

    SELECT ebeln reswk bsart
      FROM ekko
      INTO TABLE it_ekko
      WHERE ebeln = od_venda.

    IF it_ekko[] IS NOT INITIAL.

      READ TABLE it_ekko INTO wa_ekko INDEX 1.

      SELECT ebeln matnr werks inco1
        FROM ekpo
        INTO TABLE it_ekpo
        WHERE ebeln = wa_ekko-ebeln.

      SELECT ebeln parvw lifn2        "fornecedor  parceiro  ponto de  coleta
        FROM ekpa
        INTO TABLE it_ekpa
        WHERE ebeln = wa_ekko-ebeln
          AND parvw = 'PR'.

      SELECT ebeln route kunnr        "itinerário / cliente  - parceiro Local de  descarga
        FROM ekpv
        INTO TABLE it_ekpv
        WHERE ebeln = wa_ekko-ebeln.

      IF sy-ucomm EQ 'SALVAR'.
        wa_zlest0155-ag_frete = xv2+6(4).
      ENDIF.

*      XV1 = 'ZUB'.

*      PERFORM F_VALIDA_CONDICOES_PT.
    ELSE.
      CLEAR od_venda.
      erro = 1.
      MESSAGE 'OV. / Pedido não encontrado' TYPE 'I'.
    ENDIF.

  ELSE.

    SELECT vbeln route matnr werks
      FROM vbap
      INTO CORRESPONDING FIELDS OF TABLE it_vbap
      WHERE vbeln = wa_vbak-vbeln.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln.

    PERFORM f_valida_condicoes.

    IF xv2 IS NOT INITIAL.

      SELECT SINGLE *
        FROM lfa1
        INTO @DATA(wl_lfa1)
        WHERE lifnr EQ @xv2.

      ag_frete_txt = wl_lfa1-name1.

    ELSE.
      erro = 1.
      MESSAGE 'Informar Agente de Frete' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.


*======================================================================*
* Module  MODIFY_SCREEN  OUTPUT
*======================================================================*
MODULE modify_screen OUTPUT.

  DATA: ans       TYPE c.

  CASE sy-ucomm.

    WHEN 'CHANGE'.
      IF wa_ty_saida-status_aprov EQ 1.
        MESSAGE s836(sd) DISPLAY LIKE 'A' WITH 'Solicitação já foi aprovada!'.
        EXIT.
      ENDIF.

      IF wa_ty_saida-status_aprov EQ 2.
        MESSAGE s836(sd) DISPLAY LIKE 'A' WITH 'Solicitação para essa ordem foi Reprovada!'.
        EXIT.
      ENDIF.

      IF wa_ty_saida-ch_referencia IS NOT INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'A' WITH |Ct-e  emitido para a ordem. Não permitido alteração.|.
        EXIT.

      ENDIF.

      IF wa_ty_saida-status_aprov EQ 9.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Solicitação já criada para ordem. Deseja modificar?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            popup_type            = 'ICON_MESSAGE_ERROR'
            display_cancel_button = ''
          IMPORTING
            answer                = ans.
        CASE ans.
          WHEN 2 OR 'A'.
            LEAVE TO CURRENT TRANSACTION.
          WHEN 1.
            LOOP AT SCREEN.
              CASE screen-name.
                WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
                  screen-input = 1.
                  MODIFY SCREEN.
              ENDCASE.
            ENDLOOP.
        ENDCASE.
      ENDIF.

    WHEN 'CRIAR'.

      PERFORM f_valida_ordem.
      IF sy-subrc = 0.
        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
      ENDIF.


    WHEN 'BUSCAR'.

      IF motivo IS INITIAL.
        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      IF od_venda IS NOT INITIAL AND motivo IS INITIAL.

        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'VFRETE' OR 'MOTIVO'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.

      ENDIF.

      IF od_venda IS NOT INITIAL AND xv2 IS INITIAL.
        LOOP AT SCREEN.
          CASE screen-name.
            WHEN 'XV2'.
              screen-input = 1.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
      ENDIF.

  ENDCASE.

  CASE erro.
    WHEN 1.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'OD_VENDA' OR 'VFRETE' OR 'MOTIVO'.
            screen-input = 1.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

  CLEAR erro.

ENDMODULE.

*======================================================================*
* FORM F_BUSCA_OV
*======================================================================*
FORM f_valida_ordem.
  IF wa_zlest0155-status_aprov IS NOT INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe solicitação para essa Ordem!'.
    EXIT.
  ENDIF.
  IF vfrete IS NOT INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe solicitação para essa Ordem!'.
    EXIT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  SEARCH_VBELN  INPUT
*&---------------------------------------------------------------------*
FORM search_vbeln.

  CLEAR: it_return, wa_return.

  SELECT vbeln
         kunnr
         erdat
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE it_ov_busca
  ORDER BY erdat DESCENDING.

  "AJUDA (F4)
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VBELN'
      value_org       = 'S'
      window_title    = 'Pesquisa OV'
    TABLES
      value_tab       = it_ov_busca
      return_tab      = it_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE it_return INTO wa_return INDEX 1.
  od_venda = wa_return-fieldval.
ENDFORM.


FORM f_seleciona_dados_ov.

  DATA: count1  TYPE i,
        v_space TYPE string VALUE ''.

  FREE:  it_vbpa, it_lfa1, it_kna1, it_vbak, it_zsdt0011, it_vbap.
  CLEAR: wa_vbpa, wa_lfa1, wa_kna1, wa_vbak, wa_zsdt0011, wa_vbap, xv1, xv7, xv8. "XV2,

  SELECT vbeln               "Seleção dos parceiros
         parvw
         kunnr
         lifnr
    FROM vbpa
    INTO TABLE it_vbpa
    WHERE vbeln = od_venda
    AND parvw IN ('SP', 'PC', 'AG', 'LR', 'Z1').

  SELECT lifnr               "Seleção Nome do Fornecedor
         name1
         lzone
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_vbpa
    WHERE lifnr EQ it_vbpa-lifnr OR
          lifnr EQ wa_zlest0155-id_motorista.

  SELECT kunnr               "Seleção Nome do Cliente
         name1
         lzone
     FROM kna1
     INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_vbpa
     WHERE kunnr = it_vbpa-kunnr.

  SELECT vbeln               "Seleção Tipo de Ordem de Venda
         auart
     FROM vbak
     INTO TABLE it_vbak
     FOR ALL ENTRIES IN it_vbpa
     WHERE vbeln = it_vbpa-vbeln.

  SELECT tp_movimento         "Seleção Tipo de Transporte
         auart
         shtyp
    FROM zsdt0011
    INTO TABLE it_zsdt0011
    FOR ALL ENTRIES IN it_vbak
    WHERE auart = it_vbak-auart.

  SELECT vbeln                "Seleção Itinerário de Ordem de Venda
         route
    FROM vbap
    INTO TABLE it_vbap
    WHERE vbeln = od_venda.

  SELECT SINGLE pc_veiculo agregado
    FROM zlest0002
    INTO wa_zlest0002
    WHERE pc_veiculo = wa_zlest0155-ds_placa_trator.

  IF wa_zlest0002-agregado = 1.
    xv5 = '0000000001'.
  ELSE.
    xv5 = '0000000002'.
  ENDIF.

*  READ TABLE IT_ZSDT0011 INTO WA_ZSDT0011 WITH KEY TP_MOVIMENTO = 'S'.
*  XV1 = WA_ZSDT0011-SHTYP.

  LOOP AT it_vbpa INTO wa_vbpa.

    IF wa_vbpa-parvw = 'SP'.
      IF xv2 IS INITIAL.
        READ TABLE it_vbpa INTO wa_vbpa WITH KEY lifnr = wa_vbpa-lifnr.
        xv2 = wa_vbpa-lifnr.
        DATA(v_lifnr_sp) = wa_vbpa-lifnr.
      ENDIF.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa-lifnr.

    IF wa_vbpa-parvw = 'LR'.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr.
      DATA(v_kunnr_lr) = wa_kna1-kunnr.
    ENDIF.

    IF wa_vbpa-parvw = 'Z1'.
      TRY.
          DATA(v_lifnr_z1) = wa_vbpa-lifnr.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    IF wa_lfa1-lzone IS NOT INITIAL AND wa_kna1-lzone IS NOT INITIAL AND wa_vbpa-parvw = 'PC'.
      xzp = wa_lfa1-lzone. "DEVK9A1Q47 - 20.10.2023 LES - Selecionar Valor frete na zcl_calc_frete=>get_valor_frete
      xzc = wa_kna1-lzone. "DEVK9A1Q47 - 20.10.2023 LES - Selecionar Valor frete na zcl_calc_frete=>get_valor_frete
      CONCATENATE wa_lfa1-lzone wa_kna1-lzone INTO lc_zona RESPECTING BLANKS.
    ENDIF.

  ENDLOOP.

  READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbpa-vbeln.
  xv7 = wa_vbap-route.
  xv8 = '0001'.

  READ TABLE it_vbak INTO DATA(_vbak) WITH KEY vbeln = wa_vbpa-vbeln.
  IF ( sy-subrc = 0 ).
    TRY.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
          EXPORTING
            i_tipo_mov       = 'S'
            i_vsart          = '01'  "Rodoviario
            i_tipo_ov        = CONV #( _vbak-auart )
            i_parid_lr       = CONV #( v_kunnr_lr )
            i_parid_z1       = CONV #( v_lifnr_z1 )
            i_parid_sp       = CONV #( v_lifnr_sp )
          IMPORTING
             e_shtyp         = DATA(_shtyp) ).

        xv1 = _shtyp.
      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.
  ENDIF.

  CLEAR count1.
  count1 = strlen( xv7 ).
  WHILE count1 < 6.
    xv7 = |{ xv7 }{ 'A' }|.
    xv8 = | { xv8 }|.
    count1 = count1 + 1.
  ENDWHILE.
  xv7 = wa_vbap-route.
  CLEAR count1.

  xv9 = wa_cadastro-viagem_id.


ENDFORM.

FORM verifica_cte .

  SELECT vbeln
         placa_cav
         motorista
         agente_frete
         dt_movimento
         nro_nf_frete
    FROM zsdt0001
    INTO TABLE it_zsdt0001
    WHERE vbeln  	      EQ wa_ty_saida-vbeln            AND
          placa_cav     EQ wa_ty_saida-ds_placa_trator  AND
          motorista     EQ wa_ty_saida-id_motorista     AND
          agente_frete  EQ wa_ty_saida-id_branch_ag     AND
          dt_movimento  BETWEEN wa_ty_saida-dt_emissao  AND wa_ty_saida-dt_validade.

  READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY vbeln = od_venda.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_CONDICOES_PT
*&---------------------------------------------------------------------*
FORM f_valida_condicoes_pt.

  DATA: count1  TYPE i.

  READ TABLE it_ekko INTO wa_ekko INDEX 1.
  READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln.
  READ TABLE it_ekpa INTO wa_ekpa WITH KEY ebeln = wa_ekko-ebeln.
  READ TABLE it_ekpv INTO wa_ekpv WITH KEY ebeln = wa_ekko-ebeln.

  "XV1 - TP.Transporte
  SELECT SINGLE *
    FROM zsdt0011
    INTO @DATA(wl_zsdt0011)
    WHERE bsart EQ @wa_ekko-bsart.

  xv1 = wl_zsdt0011-shtyp.

  "XV2 - Ag. de Frete
  "--Preenchido na tela.

  "XV3 Zona PC - XV4 Zona LR | Concatenados na variavel LC_ZONA
  SELECT SINGLE *
    FROM lfa1
    INTO @DATA(wl_ponto_coleta)
    WHERE lifnr EQ @wa_ekpa-lifn2.

  SELECT SINGLE *
    FROM kna1
    INTO @DATA(wl_local_rec)
    WHERE kunnr EQ @wa_ekpv-kunnr.

  IF wl_ponto_coleta IS NOT INITIAL AND wl_local_rec IS NOT INITIAL.
    CONCATENATE wl_ponto_coleta-lzone wl_local_rec-lzone INTO lc_zona RESPECTING BLANKS.
  ENDIF.
  "XV5 - Agregado

  SELECT SINGLE pc_veiculo agregado
    FROM zlest0002
    INTO wa_zlest0002
    WHERE pc_veiculo = wa_zlest0155-ds_placa_trator.

  IF wa_zlest0002 IS NOT INITIAL.
    IF wa_zlest0002-agregado = 1.
      xv5 = '0000000001'.
    ELSE.
      xv5 = '0000000002'.
    ENDIF.
  ENDIF.

  "XV6 - Material
  xv6 = wa_ekpo-matnr.
  "XV7 - Rota
  xv7 = wa_ekpv-route.
  "XV8 - Itinerario
  xv8 = '0001'.

  CLEAR count1.
  count1 = strlen( xv7 ).
  WHILE count1 < 6.
    xv7 = |{ xv7 }{ 'A' }|.
    xv8 = | { xv8 }|.
    count1 = count1 + 1.
  ENDWHILE.

  xv7 = wa_ekpv-route.

  xv9 = wa_cadastro-viagem_id.

  CLEAR count1.

  CLEAR xvsoma.

  IF xpf1 IS INITIAL.
    CLEAR xvsoma.
    DATA(_lva_xv8) = xv8.
    CONDENSE _lva_xv8 NO-GAPS.
*    xvsoma = |{ xv1 }{ _lva_xv8 }{ xv9 }|.      "Tp.Transporte/contrato/viagem_id
    PERFORM f_select_valorfrete USING 'ZFRE' xv1 xv2 xv5 xv6 xv7  xv8 xv9 xzp xzc..
  ENDIF.

  IF xpf1 IS INITIAL.

    CLEAR sy-ucomm.
*    MESSAGE TEXT-019 TYPE 'I'.
*    EXIT.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_AG_FRETE  INPUT
*&---------------------------------------------------------------------*
*  Ao informar a OV, inserir o agente de frete na tela - CAMPO - XV2
*----------------------------------------------------------------------*
MODULE valida_ag_frete INPUT.

  ck_alterou_ag = abap_true.

  IF od_venda IS NOT INITIAL.
    PERFORM f_valida_ov.
  ENDIF.

  IF xv2 IS NOT INITIAL.
    PERFORM f_valida_ordem_venda.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_ordem INPUT.
  ck_alterou_oc = abap_true.

  IF od_venda IS NOT INITIAL.
    PERFORM f_valida_ov.
  ENDIF.

  IF xv2 IS NOT INITIAL.
    PERFORM f_valida_ordem_venda.
  ENDIF.

ENDMODULE.
