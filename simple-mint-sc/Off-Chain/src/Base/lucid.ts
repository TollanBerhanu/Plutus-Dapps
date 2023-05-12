import {
    Lucid,
    Blockfrost,
    Address,
    MintingPolicy,
    PolicyId,
    Unit,
    fromText,
    Data,
    getAddressDetails,
    applyParamsToScript
} from 'lucid-cardano'
// } from "https://deno.land/x/lucid@0.9.1/mod.ts"
import { blockfrostKey, secretSeed } from "./secret.js"


export const offChain = async (amount: bigint) : Promise<string> => {

    // set blockfrost endpoint
    const lucid = await Lucid.new(
        new Blockfrost(
            "https://cardano-preview.blockfrost.io/api/v0",
            blockfrostKey
        ),
        "Preview"
    );
    
    // load local stored seed as a wallet into lucid
    lucid.selectWalletFromSeed(secretSeed, {accountIndex: 1});
    const addr: Address = await lucid.wallet.address();
    console.log("own address: " + addr);

    // get my own PubKeyHash (hash of my public/payment address)
    const pkh: string = getAddressDetails(addr).paymentCredential?.hash || "";
    console.log("own pubkey hash: " + pkh); 
    
    const signedPolicy: MintingPolicy = {
        type: "PlutusV2",
        script: "5908dc5908d901000033233223322323232323232323232323232323233223232323232323222232325335323232533553353235001222222222222533533355301612001321233001225335002210031001002501d25335333573466e3c0580040c40c04d407c00454078010840c440bd4004408c4ccd5cd19b885335323301f50220013550012222222222220081301d498884d40088894cd40104004884c091264800008c088408c4cd5ce2491e4f6e6c7920746865206f776e65722063616e206d696e7420746f6b656e7300022135001220023333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd405c060d5d0a80619a80b80c1aba1500b33501701935742a014666aa036eb94068d5d0a804999aa80dbae501a35742a01066a02e0446ae85401cccd5406c08dd69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b5d69aba15002302e357426ae8940088c98c80c0cd5ce01881801709aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a816bad35742a004605c6ae84d5d1280111931901819ab9c03103002e135573ca00226ea8004d5d09aba2500223263202c33573805a05805426aae7940044dd50009aba1500533501775c6ae854010ccd5406c07c8004d5d0a801999aa80dbae200135742a00460426ae84d5d1280111931901419ab9c029028026135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660226ae84d5d1280191931900d19ab9c01b01a0183333573466e1cd55ce9baa0044800080648c98c8064cd5ce00d00c80b880c09931900c19ab9c49010350543500018135573ca00226ea8004c8004d5406c88448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80d911999aab9f0012501b233501a30043574200460066ae880080508c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00a80a00909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500d014357426ae8940088c98c8064cd5ce00d00c80b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7007006c06406005c4d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201533573802c02a02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355018223233335573e0044a032466a03066442466002006004600c6aae754008c014d55cf280118021aba200301213574200224464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01301201000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01101000e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00d00c00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200d23333573466e1d40092000200d23263200633573800e00c00800626aae74dd5000a4c2400292010350543100248000c8004d540188894cd40044008884d400888cc01cccc02000801800400cc8004d5401488894cd40044008884d4008894cd4ccd5cd19b870014800003002c4ccc02001c01800c4ccc02001ccd402448ccc00402000c00801800c4488008488488cc00401000c488008488004448c8c00400488cc00cc0080080052211c6cec697e064de530cef331f0a17358c0e7a47f96343ec4f8b015b38a0001",
    };

    // This only works for parameters of type BuiltinData (in the on-chain code)
    
    // const Params = Data.Tuple([Data.String]); // Tell lucid what type the parameter will have. Data.Tuple is used because we can have >1 param
    // type Params = Data.Static<typeof Params>;
    // const signedPolicy: MintingPolicy = {
    //     type: "PlutusV2",
    //     script: applyParamsToScript<Params>(
    //         "590b5f590b5c010000323232323233223232323232323232332232323322323232323232323232323233322232323232222323253353232323253353235001222222222222533533355301f12001335023225335002210031001502d25335333573466e3c0480040e40e04d40bc004540b8010840e440dd400440ac54cd4c8ccd54c04c48004d405940548d400488ccd54c05848004d406540608d400488ccd5cd19b87480000040c80c4004004d4c8c8d4cd54054c8c8cd54c054480048d400488cd54094008cd54c060480048d400488cd540a0008ccd40048cc0e92000001223303b00200123303a00148000004cd54c054480048d400488cd54094008ccd40048cd54c064480048d400488cd540a4008d5406c00400488ccd55404c0540080048cd54c064480048d400488cd540a4008d54064004004ccd554038040008004d5400888cd54c058480048d400488cd54098008cd54c064480048d400488cd540a4008cdc124002002002004a03e6aa002440024466aa04400400226a66aaaa01c46466aa0246aa002440046aa0024400226a66aaaa01e4a66a666ae68cdc4800a400005c05a26aa02200226aa0260020024466aa0440040020024466aa0420040026aa002444444444444010440022056266ae71241212a2a2a20496e76616c6964207369676e617475726520746f204d696e74202a2a2a0002a135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4090094d5d0a80619a8120129aba1500b33502402635742a014666aa050eb9409cd5d0a804999aa8143ae502735742a01066a0480646ae85401cccd540a00cdd69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40f5d69aba15002303e357426ae8940088c98c8108cd5ce02182102009aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81ebad35742a004607c6ae84d5d1280111931902119ab9c043042040135573ca00226ea8004d5d09aba2500223263203e33573807e07c07826aae7940044dd50009aba1500533502475c6ae854010ccd540a00bc8004d5d0a801999aa8143ae200135742a00460626ae84d5d1280111931901d19ab9c03b03a038135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860426ae84d5d1280211931901619ab9c02d02c02a3333573466e1cd55ce9baa0054800080ac8c98c80accd5ce0160158149bae00510291326320293357389210350543500029135573ca00226ea8004444888ccd54c010480054054cd54c028480048d400488cd54068008d54030004ccd54c0104800488d4008894cd4ccd54c03c48004c8cd405088ccd400c88008008004d40048800448cc004894cd400840ac40040a08d400488cc028008014018400c4cd406401000d4058004cd54c028480048d400488c8cd5406c00cc004014c8004d540b0894cd40044d5403000c884d4008894cd4cc0300080204cd5404401c0044c01800c008c8004d5409488448894cd40044008884cc014008ccd54c01c4800401401000488ccd5cd19b8f0020010210201111223233550093550012200235500122001133335530071200133500b222300330020012001223500222350032233350032335501a003335018335501a00500100222335501b335019335501b006002004335019335501b0060010032335501a335018335501a00500100300233553006120012350012233550160023005001001335501350125012112122230030041122122233002005004112122230010043200135501f221122533500115010221335011300400233553006120010040013200135501e22112225335001135003220012213335005220023004002333553007120010050040011122123300100300211233001225335002100110190181233500222333500322002002001350012200112212330010030021232230023758002640026aa034446666aae7c004940288cd4024c010d5d080118019aba2002019232323333573466e1cd55cea80124000466442466002006004601e6ae854008c014d5d09aba2500223263201933573803403202e26aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460306ae854008cd404005cd5d09aba2500223263201e33573803e03c03826aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931901019ab9c02102001e01d01c135573aa00226ea8004d5d0a80119a8063ae357426ae8940088c98c8068cd5ce00d80d00c09aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405c88c8cccd55cf80112804119a80399aa80498031aab9d5002300535573ca00460086ae8800c05c4d5d08008891001091091198008020018891091980080180109119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402866ae700540500480444d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402466ae7004c04804003c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401c66ae7003c0380304d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200c33573801a01801426ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201533573802c02a02602402202001e01c01a26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900719ab9c00f00e00c00b135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c802ccd5ce00600580480409aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900619ab9c00d00c00a009008135573aa00226ea80048c8cccd5cd19b8750014800880148cccd5cd19b8750024800080148c98c8020cd5ce00480400300289aab9d37540022440042440029309000a4810350543100223370000400222464600200244660066004004003",
    //         [pkh], // Here, we apply our PKH to the serialized minting script to make it a complete minting policy
    //         Params)
    // };
    
    const policyId: PolicyId = lucid.utils.mintingPolicyToId(signedPolicy); // get the CurrencySymbol
    console.log("minting policy: " + policyId);
    
    const unit: Unit = policyId + fromText("Mint Burn"); // represent the AssetClass (CurrencySymbol, TokenName)
    
    const tx = await lucid // construct the txn
        .newTx()
        .mintAssets({[unit]: amount}, Data.void())
        .attachMintingPolicy(signedPolicy)
        .addSignerKey(pkh) // we need to explicitly state the pubKeyHash of the signer
        .complete();
    
    const signedTx : any = await tx.sign().complete(); // We sign it using out privKey and submit it
    // const txHash = await signedTx.submit();
    console.log("tid: " + signedTx);
    
    return signedTx

}