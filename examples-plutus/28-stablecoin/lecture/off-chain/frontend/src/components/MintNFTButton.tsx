import { PolicyId, UTxO, Unit } from "lucid-cardano";
import React, { useContext } from "react";
import {
    applyParamsToScript,
    Data,
    MintingPolicy,
    fromText,
} from "lucid-cardano";
import { AppStateContext } from "@/pages/_app";
import { signAndSubmitTx } from "@/utilities/utilities";

export default function MintNFT() {
    const { appState, setAppState } = useContext(AppStateContext);
    const { lucid, wAddr, nftPolicyIdHex } = appState;

    const getUtxo = async (address: string): Promise<UTxO> => {
        const utxos = await lucid!.utxosAt(address);
        const utxo = utxos[0];
        return utxo;
    };

    type GetFinalPolicy = {
        nftPolicy: MintingPolicy;
        unit: Unit;
    };

    const getFinalPolicy = async (utxo: UTxO): Promise<GetFinalPolicy> => {
        const tn = fromText("Oracle's NFT");
        const Params = Data.Tuple([Data.Bytes(), Data.Integer(), Data.Bytes()]);
        type Params = Data.Static<typeof Params>;
        const nftPolicy: MintingPolicy = {
            type: "PlutusV2",
            script: applyParamsToScript<Params>(
                "5908e55908e20100003233223322323222222532335323232323232323232323232323232323232323232325332355335300535500122222222222200c10231022153355335323301f5022001355001222222222222008102222135002222533500415335333573466e3c0080240a009c4ccd5cd19b87001480080a009c409c8840a4408c40884d4004880084084c8c8cccd5cd19b87001480008c8c8c8cc8848cc00400c008c8c8cccd5cd19b87001480008c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd40cc0a4058cd40cc0a4050cd40cc0b0048ccd540c9d7018808199aa8193ae03100e33503302800c3335503202d75a01464646666ae68cdc3800a400046464646644246600200600464646666ae68cdc3800a400046464646644246600200600466a064eb4008c0c4004d5d09aba2500235742a00226aae7800c8c98c80e8cd5ce01c01f81c1aab9d001375400464646666ae68cdc3800a400046464646644246600200600466a064eb4008c0c4004d5d09aba2500235742a00226aae7800c8c98c80e8cd5ce01c01f81c1aab9d00137540026ae84d5d128011aba15001135573c006464c6406866ae700c80e40c8d55ce8009baa00833503375c00c666aa06404e048008666aa064eb8090008c0ac004d5d09aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226aae7800c8c98c8068cd5ce00c00f80c1aab9d0013754004601a0026ae84d5d128011aba15001135573c006464c6402866ae70048064048d55ce8009baa01732323333573466e1c005200020182326320133357380220300226aae74004dd500b9bae01735501f225335001101f2215335325335333573466e3cd400488008d40588800808c0884ccd5cd19b8735001220013501622001023022102235002220021022130040012001232323333573466e1c0052002202023333573466e1c0092000202023263201133573801e02c01e01c6aae74004dd500089119191999ab9a3370e00290021091100091999ab9a3370e0049001119190911180180218038009aba135573c00846666ae68cdc3801a400042444004464c6402466ae7004005c04003c038d55ce8009baa001232323333573466e1c0052006232321222230040053006001357426aae7800c8cccd5cd19b87002480108c8c848888c008014c030004d5d09aab9e00423333573466e1c00d200223232122223001005300a001357426aae780148cccd5cd19b87004480008c8c848888c00c014dd70009aba135573c00c464c6402266ae7003c05803c038034030d55ce8009baa001232323333573466e1c005200c21222222200323333573466e1c009200a21222222200423333573466e1c00d2008232323233221222222233001009008375c0046eb4004d5d09aba2500235742a00226aae780148cccd5cd19b87004480188c8c8c8cc8848888888cc008024020dd70011bae001357426ae894008d5d0a80089aab9e00623333573466e1c01520042323232332212222222330060090083010002375c0026ae84d5d128011aba15001135573c00e46666ae68cdc3803240044646424444444600e010601e0026ae84d55cf00411999ab9a3370e00e90001191909111111180280418080009aba135573c012464c6402666ae7004406004404003c03803403002cd55ce8009baa001232323333573466e1c00520002323232332212330010030023008002300a001357426ae894008d5d0a80089aab9e00323263200c3357380140220146aae74004dd5000919191999ab9a3370e002900011919191980918040011bad001357426ae894008d5d0a80089aab9e00323263200b3357380120200126aae74004dd5000919191999ab9a3370e00290001191bae001357426aae7800c8c98c8028cd5ce0040078041aab9d0013754002464646666ae68cdc3800a4000464646464646464646666444424666600200a00800600464646666ae68cdc3800a4000464646466442466002006004602800466a00e0260026ae84d5d128011aba15001135573c006464c6402866ae70048064048d55ce8009baa0073335501275c02200a64646666ae68cdc3800a4008464244460040086ae84d55cf00191999ab9a3370e004900111919091118008021bae001357426aae780108cccd5cd19b87003480008488800c8c98c8058cd5ce00a00d80a0098091aab9d001375400666a002eb8008488c8c8cccd5cd19b87001480008c8c8488c00800cc018004d5d09aab9e00323333573466e1c009200221220012326320163357380280360280266aae74004dd50009aba135744a0046ae8540044d5d128011aba15001135744a0046ae8540044d55cf00191931900499ab9c00700e00735573a0026ea80048c8c8cccd5cd19b87001480088c8c8c8c8c8ccc888488ccc00401401000cdd68021bad002375a0026ae84d5d128011aba15001135744a0046ae8540044d55cf00191999ab9a3370e004900011919091180100198038009aba135573c008464c6401266ae7001c03801c018d55ce8009baa001232323333573466e1c0052002232321223001003375c0026ae84d55cf00191999ab9a3370e00490001191909118010019bae001357426aae780108c98c8020cd5ce0030068030029aab9d001375400292103505431004988ccd54009d73ad0011122322300237560026aa024446666aae7c004940488cd4044c8cc8848cc00400c008c018d55cea80098029aab9e500113574200460066ae8800802048c88c008dd60009aa808111999aab9f00125010233500f30043574200460066ae88008018cc004dd70039bad006221233001003002100116120013550062225335001100222135002223300733300800200600100335500522225335001100222135002225335333573466e1c005200000c00b1333008007006003133300800733500912333001008003002006003112200212212233001004003122002122001112323001001223300330020020011",
                [utxo.txHash, BigInt(utxo.outputIndex), tn],
                Params
            ),
        };
        const policyId: PolicyId = lucid!.utils.mintingPolicyToId(nftPolicy);
        const unit: Unit = policyId + tn;
        setAppState({
            ...appState,
            nftPolicyIdHex: policyId,
            nftTokenNameHex: tn,
            nftAssetClassHex: unit,
            nftPolicy: nftPolicy,
        });

        return { nftPolicy, unit };
    };

    const mintNFT = async () => {
        console.log("minting NFT for " + wAddr);
        if (wAddr) {
            const utxo = await getUtxo(wAddr);
            const { nftPolicy, unit } = await getFinalPolicy(utxo);

            const tx = await lucid!
                .newTx()
                .mintAssets({ [unit]: 1n }, Data.void())
                .attachMintingPolicy(nftPolicy)
                .collectFrom([utxo])
                .complete();

            await signAndSubmitTx(tx);
        }
    };

    return (
        <button
            onClick={mintNFT}
            disabled={!wAddr || !!nftPolicyIdHex}
            className=" bg-zinc-800 text-white font-quicksand text-lg font-bold py-3 px-8 rounded-lg shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600"
        >
            {" "}
            Mint Oracle&apos;s NFT
        </button>
    );
}