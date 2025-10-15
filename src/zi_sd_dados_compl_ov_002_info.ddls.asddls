@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados Complementares OV'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SD_DADOS_COMPL_OV_002_INFO
  as select from    vbak                          as OV
  -- Fluxo 1
    left outer join zsdt0041                      as A1 on OV.vbeln = A1.vbeln
    left outer join zsdt0040                      as A2 on A1.doc_simulacao = A2.doc_simulacao

  -- Fluxo 2
    left outer join zsdt0090                      as B1 on OV.vbeln = B1.vbelv
    left outer join zsdt0040                      as B2 on B1.doc_simulacao = B2.doc_simulacao

  -- Fluxo 3
    left outer join zsdt0090                      as C1 on OV.vbeln = C1.vbeln
    left outer join zsdt0040                      as C2 on C1.doc_simulacao = C2.doc_simulacao

  -- Fluxo 4
    left outer join zsdt0053                      as D1 on OV.vbeln = D1.vbeln
    left outer join zsdt0051                      as D2 on D1.nro_sol_ov = D2.nro_sol_ov

  -- Fluxo 5
    left outer join zsdt0066                      as E1 on OV.vbeln = E1.vbeln

  -- Fluxo 6
    left outer join vbfa                          as F1 on  OV.vbeln   = F1.vbeln
                                                        and F1.vbtyp_v = 'C' -- Ordem


    left outer join zsdt0053                      as F2 on F1.vbelv = F2.vbeln
    left outer join zsdt0051                      as F3 on F2.nro_sol_ov = F3.nro_sol_ov

    association [0..*] to ZI_SD_DADOS_COMPL_OV_001_INFO as _item_max on OV.vbeln = _item_max.vbeln

{
  OV.vbeln,


  case
    when COALESCE(A2.tpsim,'') <> '' then
       A2.tpsim       -- Fluxo 1
    when COALESCE(B2.tpsim,'') <> '' then
       B2.tpsim
    when COALESCE(C2.tpsim,'') <> '' then
       C2.tpsim
    end as TPSIM,


  case
  when COALESCE(D2.auart,'') <> '' then
     D2.auart -- Fluxo 4   
  when COALESCE(A1.auart,'') <> '' then
     A1.auart -- Fluxo 1  
  else
     OV.auart
  end   as AUART,


  case
  when COALESCE(D1.matnr,'') <> '' then
    D1.matnr -- fluxo 4
  when COALESCE(A1.matnr,'') <> '' then
    A1.matnr -- Fluxo 1  
  when COALESCE(E1.matnr,'') <> '' then
    E1.matnr -- fluxo 5
  when COALESCE(F2.matnr,'') <> '' then
    F2.matnr -- fluxo 6
  when COALESCE(_item_max.matnr,'') <> '' then
    _item_max.matnr
  end   as MATNR,

  case
  when COALESCE(D1.nro_sol_ov,'') <> '' then
     D1.nro_sol_ov    -- Fluxo 4
  when COALESCE(A1.doc_simulacao,'') <> '' then
     A1.doc_simulacao -- Fluxo 1
  when COALESCE(B2.doc_simulacao,'') <> '' then
     B2.doc_simulacao -- Fluxo 2
  when COALESCE(C2.doc_simulacao,'') <> '' then
     C2.doc_simulacao -- Fluxo 3  
  when COALESCE(F2.nro_sol_ov,'') <> '' then
     F2.nro_sol_ov    -- Fluxo 6
  end   as NRO_SOL,


  case
  when COALESCE(D2.tp_venda,'') <> '' then
    D2.tp_venda
  when COALESCE(F3.tp_venda,'') <> '' then
    F3.tp_venda
  end   as TP_VENDA,
  
  _item_max.charg as charg, 

  _item_max
}
