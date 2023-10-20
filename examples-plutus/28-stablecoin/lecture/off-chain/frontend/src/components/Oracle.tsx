import { AppStateContext } from "@/pages/_app";
import { signAndSubmitTx } from "@/utilities/utilities";
import {
    PaymentKeyHash,
    SpendingValidator,
    UTxO,
    getAddressDetails,
} from "lucid-cardano";
import { applyParamsToScript, Data } from "lucid-cardano";
import { useContext, useEffect, useState } from "react";

const OracleRedeemer = Data.Enum([
    Data.Literal("Update"),
    Data.Literal("Delete"),
]);
type OracleRedeemer = Data.Static<typeof OracleRedeemer>;

export default function Oracle() {
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        lucid,
        wAddr,
        nftPolicyIdHex,
        nftTokenNameHex,
        nftAssetClassHex,
        oracleWithNftUTxO,
        oracleScript,
        oracleAddress,
    } = appState;
    const [rate, setRate] = useState(100n);
    const [count, setCount] = useState(0);

    useEffect(() => {
        getOracleNftUtxO();
        setTimeout(() => setCount(count + 1), 5e3);
    }, [count]);

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////

    const getOracleNftUtxO = async () => {
        if (lucid && wAddr && oracleAddress) {
            const oracUtxO = await lucid.utxosAt(oracleAddress).catch((err) => {
                console.log("Can't find Oracle UtxO");
            });
            if (!oracUtxO) return;
            const oracWithNftUTxO = oracUtxO.find((utxo: UTxO) => {
                return Object.keys(utxo.assets).some((key) => {
                    return key == nftAssetClassHex;
                });
            });
            if (
                oracWithNftUTxO == undefined ||
                oracWithNftUTxO == oracleWithNftUTxO
            )
                return;
            setAppState({
                ...appState,
                oracleWithNftUTxO: oracWithNftUTxO,
            });
        }
    };

    const parseRate = (r: string) => {
        const rate = BigInt(Number(r));
        if (Number.isNaN(rate)) return;
        setRate(rate);
    };

    const getFinalScript = async (
        pkh: PaymentKeyHash
    ): Promise<SpendingValidator | undefined> => {
        console.log("Deploying Oracle with Rate and AssetClass: ", {
            rate,
            nftPolicyIdHex,
            nftTokenNameHex,
        });
        if (!lucid || !nftPolicyIdHex || !nftTokenNameHex) return;

        const Params = Data.Tuple([Data.Bytes(), Data.Bytes(), Data.Bytes()]);
        type Params = Data.Static<typeof Params>;
        const oracleScript: SpendingValidator = {
            type: "PlutusV2",
            script: applyParamsToScript<Params>(
                "590b0f590b0c010000323233223232323232323232323232332232323322323232323232323222222253353232323253232335004153353300150023500622001101a101915332355335333573466e1cd4d401c8800888ccc00cd54cd4c05c01884d40048800458888800c008005200201b01a101b101a15332355335333573466e1cd4d40208800888ccc010d5400c888800c008005200201c01b101c101b1533553353300350043500822001101c101b153355335323253233353500322220021502b2130020012321533535004222222222222300d0022130040011502d35502f2253350011502e22135002225335333573466e3c00801c0980944d40cc0044c01800c8ccccccd5d200092816128161281611a8169bad0022502c029500550012101d101b101c101b101b15335533530150042135001223500122223500b2235002222222222222333553023120012235002222253353501822350062232335005233500425335333573466e3c0080040f40f05400c40f080f08cd401080f094cd4ccd5cd19b8f00200103d03c15003103c1533500321533500221335002233500223350022335002233039002001203f2335002203f23303900200122203f222335004203f2225335333573466e1c01800c10810454cd4ccd5cd19b870050020420411333573466e1c0100041081044104410440e854cd4004840e840e84cd40c4018014401540b002858588854cd4004400888584068888c8cd540b4894cd40045200022135002225335333573466e3c00802409008c4c01c0044c01800c010d540b0894cd40045200022135002225335333573466e3c00801c08c08840044c01800c406488c8d400c8888888888894cd4c034010840a4409cd540a8894cd4004540a48854cd4ccd5cd19b8f00500201f01e13502c002130040011350012200232323333573466e1c005200023232323322123300100300232323333573466e1c0052000232323232323232323232323232323232323232323232323333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502f02b01633502f02b01433502f02c0123335502e75c05a020666aa05ceb80b4038cd40bc0e8030ccd540b80edd680519191999ab9a3370e002900011919191991091980080180119191999ab9a3370e002900011919191991091980080180119a8223ad0023041001357426ae894008d5d0a80089aab9e00323263204c3357380920940946aae74004dd500119191999ab9a3370e002900011919191991091980080180119a8223ad0023041001357426ae894008d5d0a80089aab9e00323263204c3357380920940946aae74004dd50009aba135744a0046ae8540044d55cf00191931902319ab9c04304404435573a0026ea8020cd40bdd7003199aa81701c81a002199aa8173ae034002303e001357426ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aba2500235742a00226ae894008d5d0a80089aab9e00323263202c3357380520540546aae74004dd5001180f8009aba135744a0046ae8540044d55cf00191931901319ab9c02302402435573a0026ea8010c8c8cccd5cd19b870014800884880088cccd5cd19b870024800084880048c98c8098cd5ce0118120120119aab9d00137540086eb4010cc8848cc00400c008cd54088dd70031bae005375c008203c2c6aa0384422444a66a00220044426600a004666aa600e2400200a008002464646666ae68cdc3800a4000464646466442466002006004603200460100026ae84d5d128011aba15001135573c006464c6403866ae70064068068d55ce8009baa001232323333573466e1c00520002323232323232323333222212333300100500400300232323333573466e1c00520002323232332212330010030023021002335024020001357426ae894008d5d0a80089aab9e0032326320253357380440460466aae74004dd5003199aa806bae00c00432323333573466e1c005200423212223002004357426aae7800c8cccd5cd19b87002480088c8c84888c004010dd70009aba135573c00846666ae68cdc3801a400042444006464c6404e66ae7009009409409008cd55ce8009baa00233501e75c0026ae84d5d128011aba15001135744a0046ae8540044d5d128011aba15001135573c006464c6403666ae70060064064d55ce8009baa00123335500275ceb40044488c88c008dd58009aa80d111999aab9f0012500a233500932335501d300635573aa002600a6aae7940044d5d080118019aba200201712322300237580026aa030446666aae7c004940208cd401cc010d5d080118019aba200201522333573466e3c0080040240208d400488d4008888888888888cccd40349408894088940888cd54094894cd400454090884d4008894cd54cd4ccd5cd19b8f3500222002350072200201c01b1333573466e1cd400888004d401c8800407006c406c4d40a40104c01800c0344488008488488cc00401000c80048c8c8cccd5cd19b870014800880188cccd5cd19b870024800080188c98c804ccd5ce0080088088081aab9d001375400224400424400224464646666ae68cdc3800a40084244400246666ae68cdc3801240044646424446006008600e0026ae84d55cf00211999ab9a3370e00690001091100111931900919ab9c00f01001000f00e35573a0026ea80048c8c8cccd5cd19b87001480188c8c848888c010014c018004d5d09aab9e00323333573466e1c0092004232321222230020053008001357426aae780108cccd5cd19b87003480088c8c848888c004014c02c004d5d09aab9e00523333573466e1c011200023232122223003005375c0026ae84d55cf00311931900899ab9c00e00f00f00e00d00c35573a0026ea80048c8c8cccd5cd19b870014803084888888800c8cccd5cd19b87002480288488888880108cccd5cd19b87003480208c8c8c8cc8848888888cc004024020dd70011bad001357426ae894008d5d0a80089aab9e00523333573466e1c0112006232323233221222222233002009008375c0046eb8004d5d09aba2500235742a00226aae780188cccd5cd19b87005480108c8c8c8cc8848888888cc018024020c030008dd70009aba135744a0046ae8540044d55cf00391999ab9a3370e00c90011191909111111180380418058009aba135573c01046666ae68cdc3803a40004646424444444600a01060180026ae84d55cf00491931900999ab9c01001101101000f00e00d00c00b35573a0026ea80048c8c8cccd5cd19b87001480088c8c8c8c8c8ccc888488ccc00401401000cdd68021bad002375a0026ae84d5d128011aba15001135744a0046ae8540044d55cf00191999ab9a3370e004900011919091180100198038009aba135573c008464c6401a66ae7002802c02c028d55ce8009baa001232323333573466e1c0052002232321223001003375c0026ae84d55cf00191999ab9a3370e00490001191909118010019bae001357426aae780108c98c8030cd5ce0048050050049aab9d0013754002464646666ae68cdc3800a400046464646644246600200600460100046eb4004d5d09aba2500235742a00226aae7800c8c98c8028cd5ce0038040041aab9d0013754002464646666ae68cdc3800a40004646eb8004d5d09aab9e00323263200933573800c00e00e6aae74004dd500089119191999ab9a3370e00290001191a80598030009aba135573c00646666ae68cdc3801240044a014464c6401466ae7001c02002001cd55ce8009baa001490103505431001200149848488c00800c4488004448848cc00400c008448c8c00400488cc00cc0080080041",
                [nftPolicyIdHex, nftTokenNameHex, pkh],
                Params
            ),
        };
        return oracleScript;
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// DEPLOY ORACLE ///////////////////////////////////////////

    const deployOracle = async () => {
        if (!lucid || !wAddr) {
            alert("Please connect account and mint NFT!");
            return;
        }
        const pkh: string =
            getAddressDetails(wAddr).paymentCredential?.hash || "";
        const oracle = await getFinalScript(pkh);
        if (!oracle || !nftAssetClassHex) {
            alert("Please mint NFT first!");
            return;
        }
        const oracleAddress = lucid!.utils.validatorToAddress(oracle);
        console.log("final oracle script: ", oracle);
        console.log("final oracle address: ", oracleAddress);
        setAppState({
            ...appState,
            oracleScript: oracle,
            oracleAddress: oracleAddress,
        });

        const tx = await lucid!
            .newTx()
            .payToContract(
                oracleAddress,
                { inline: Data.to(rate, Data.Integer()) },
                { [nftAssetClassHex]: 1n }
            )
            .addSignerKey(pkh)
            .complete();
        await signAndSubmitTx(tx);
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// UPDATE ORACLE ///////////////////////////////////////////

    const updateOracle = async () => {
        if (
            wAddr &&
            lucid &&
            nftAssetClassHex &&
            oracleScript &&
            oracleWithNftUTxO &&
            oracleAddress
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const tx = await lucid!
                .newTx()
                .collectFrom(
                    [oracleWithNftUTxO], // UTXO to spend
                    Data.to<OracleRedeemer>("Update", OracleRedeemer) // Redeemer
                )
                .payToContract(
                    oracleAddress,
                    { inline: Data.to(rate, Data.Integer()) },
                    { [nftAssetClassHex]: 1n }
                )
                .attachSpendingValidator(oracleScript)
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert("Please, deploy the oracle before updating it!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////// DELETE ORACLE ///////////////////////////////////////////

    const deleteOracle = async () => {
        if (
            wAddr &&
            lucid &&
            nftAssetClassHex &&
            oracleScript &&
            oracleWithNftUTxO &&
            oracleAddress
        ) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            const tx = await lucid!
                .newTx()
                .collectFrom(
                    [oracleWithNftUTxO], // UTXO to spend
                    Data.to<OracleRedeemer>("Delete", OracleRedeemer) // Redeemer
                )
                .payToAddress(wAddr, { [nftAssetClassHex]: 1n })
                .attachSpendingValidator(oracleScript)
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert(
                "You have to deploy the oracle before being able to delete it!"
            );
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////// UI /////////////////////////////////////////////////

    return (
        <div className="w-full">
            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p>Current price of ADA (in USD cents):</p>
                <input
                    type="number"
                    value={Number(rate)}
                    onChange={(e) => parseRate(e.target.value)}
                    className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
                />
            </div>
            <div className="w-full flex flex-row gap-4">
                <button
                    onClick={deployOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !!oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Deploy Oracle
                </button>
                <button
                    onClick={updateOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Update Oracle
                </button>
                <button
                    onClick={deleteOracle}
                    disabled={
                        !lucid ||
                        !wAddr ||
                        !nftAssetClassHex ||
                        rate === 0n ||
                        !oracleWithNftUTxO
                    }
                    className="w-full rounded-lg p-3 text-zinc-50 disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold bg-red-400 active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                    {" "}
                    Delete Oracle
                </button>
            </div>
        </div>
    );
}
