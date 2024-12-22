using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using CLIPSNET;


namespace ClipsFormsExample
{
    public partial class ClipsFormsExample : Form
    {
        private CLIPSNET.Environment clips = new CLIPSNET.Environment();
        /// <summary>
        /// Распознаватель речи
        /// </summary>
        private Microsoft.Speech.Synthesis.SpeechSynthesizer synth;
        
        /// <summary>
        /// Распознавалка
        /// </summary>
        private Microsoft.Speech.Recognition.SpeechRecognitionEngine recogn;

        public ClipsFormsExample()
        {
            InitializeComponent();
            this.WindowState = FormWindowState.Maximized;
            checkedListBox1.SetItemChecked(0, true);
            checkedListBox2.SetItemChecked(0, true);
            checkedListBox3.SetItemChecked(0, true);
            checkedListBox4.SetItemChecked(0, true);
            checkedListBox5.SetItemChecked(0, true);
            checkedListBox6.SetItemChecked(0, true);
        }

        private void NewRecognPhrases(List<string> phrases)
        {
            outputBox.Text += "Стартуем распознавание" + System.Environment.NewLine;
            var Choises = new Microsoft.Speech.Recognition.Choices();
            Choises.Add(phrases.ToArray());

            var gb = new Microsoft.Speech.Recognition.GrammarBuilder();
            var RecognizerInfo = Microsoft.Speech.Recognition.SpeechRecognitionEngine.InstalledRecognizers().Where(ri => ri.Culture.Name == "ru-RU").FirstOrDefault();
            gb.Culture = RecognizerInfo.Culture;
            gb.Append(Choises);

            var gr = new Microsoft.Speech.Recognition.Grammar(gb);
            recogn.LoadGrammar(gr);
            recogn.RequestRecognizerUpdate();
            recogn.RecognizeAsync(Microsoft.Speech.Recognition.RecognizeMode.Multiple);
        }

        private void Recogn_SpeechRecognized(object sender, Microsoft.Speech.Recognition.SpeechRecognizedEventArgs e)
        {
            recogn.RecognizeAsyncStop();
            recogn.RecognizeAsyncCancel();
            outputBox.Text += "Ваш голос распознан!" + System.Environment.NewLine;
            clips.Eval("(assert (answer " + e.Result.Text + "))");
            clips.Eval("(assert (clearmessage))");
            outputBox.Text += "Продолжаю выполнение!" + System.Environment.NewLine;
            clips.Run();
            HandleResponse();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
        }

        private void HandleResponse()
        {
            outputBox.Clear();
            //  Вытаскиаваем факт из ЭС
            String evalStr = "(find-fact ((?f ioproxy)) TRUE)";
            FactAddressValue fv = (FactAddressValue)((MultifieldValue)clips.Eval(evalStr))[0];

            MultifieldValue damf = (MultifieldValue)fv["messages"];
            MultifieldValue vamf = (MultifieldValue)fv["answers"];

            outputBox.Text += System.Environment.NewLine + "Новая итерация : " + System.Environment.NewLine;
            for (int i = 0; i < damf.Count; i++)
            {
                LexemeValue da = (LexemeValue)damf[i];
                byte[] bytes = Encoding.Default.GetBytes(da.Value);
                string message = Encoding.UTF8.GetString(bytes);
                //synth.SpeakAsync(message);
                outputBox.Text += message + System.Environment.NewLine + System.Environment.NewLine;
            }

            var phrases = new List<string>();
            if (vamf.Count > 0)
            {
                outputBox.Text += "----------------------------------------------------" + System.Environment.NewLine;
                for (int i = 0; i < vamf.Count; i++)
                {
                    //  Варианты !!!!!
                    LexemeValue va = (LexemeValue)vamf[i];
                    byte[] bytes = Encoding.Default.GetBytes(va.Value);
                    string message = Encoding.UTF8.GetString(bytes);
                    phrases.Add(message);
                    outputBox.Text += "Добавлен вариант для распознавания " + message + System.Environment.NewLine;
                }
            }
            
            if(vamf.Count == 0)
                clips.Eval("(assert (clearmessage))");
            else
                NewRecognPhrases(phrases);
        }

        private void nextBtn_Click(object sender, EventArgs e)
        {
            clips.Clear();
            clips.LoadFromString(codeBox.Text);
            clips.Reset();

            if (checkedListBox1.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox1.Items[0].ToString().Trim()}\") (certainty {((float)TAScert.Value).ToString().Replace(",", ".").Trim()})))");
            if (checkedListBox1.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox1.Items[1]}\") (certainty {((float)TAWcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox1.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox1.Items[2]}\") (certainty {((float)TABcert.Value).ToString().Replace(",", ".")})))");

            if (checkedListBox2.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox2.Items[0]}\") (certainty {((float)FSScert.Value).ToString().Replace(",", ".")})))");
            else if (checkedListBox2.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox2.Items[1]}\") (certainty {((float)FSFcert.Value).ToString().Replace(",", ".")})))");
            else if (checkedListBox2.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox2.Items[2]}\") (certainty {((float)FSCcert.Value).ToString().Replace(",", ".")})))");

            if (checkedListBox3.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox3.Items[0]}\") (certainty {((float)CRcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox3.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox3.Items[1]}\") (certainty {((float)CMcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox3.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox3.Items[2]}\") (certainty {((float)COcert.Value).ToString().Replace(",", ".")})))");

            if (checkedListBox4.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox4.Items[0]}\") (certainty {((float)CGNcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox4.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox4.Items[1]}\") (certainty {((float)CGRcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox4.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox4.Items[2]}\") (certainty {((float)CGOcert.Value).ToString().Replace(",", ".")})))");

            if (checkedListBox5.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox5.Items[0]}\") (certainty {((float)TCNcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox5.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox5.Items[1]}\") (certainty {((float)TCRcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox5.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox5.Items[2]}\") (certainty {((float)TCOcert.Value).ToString().Replace(",", ".")})))");

            if (checkedListBox6.GetItemChecked(0))
                clips.Eval($"(assert (input-question (name \"{checkedListBox6.Items[0]}\") (certainty {((float)TNNcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox6.GetItemChecked(1))
                clips.Eval($"(assert (input-question (name \"{checkedListBox6.Items[1]}\") (certainty {((float)TNRcert.Value).ToString().Replace(",", ".")})))");
            if (checkedListBox6.GetItemChecked(2))
                clips.Eval($"(assert (input-question (name \"{checkedListBox6.Items[2]}\") (certainty {((float)TNOcert.Value).ToString().Replace(",", ".")})))");

            /*
            clips.Eval($"(assert (input-question (name \"{checkedListBox1.Items[checkedListBox1.CheckedIndices[0]]}\")))");
            clips.Eval($"(assert (input-question (name \"{checkedListBox2.Items[checkedListBox2.CheckedIndices[0]]}\")))");
            clips.Eval($"(assert (input-question (name \"{checkedListBox3.Items[checkedListBox3.CheckedIndices[0]]}\")))");
            clips.Eval($"(assert (input-question (name \"{checkedListBox4.Items[checkedListBox4.CheckedIndices[0]]}\")))");
            clips.Eval($"(assert (input-question (name \"{checkedListBox5.Items[checkedListBox5.CheckedIndices[0]]}\")))");
            clips.Eval($"(assert (input-question (name \"{checkedListBox6.Items[checkedListBox6.CheckedIndices[0]]}\")))");
            */
            clips.Run();

            HandleResponse();
        }

        private void resetBtn_Click(object sender, EventArgs e)
        {
            outputBox.Text = "Выполнены команды Clear и Reset." + System.Environment.NewLine;
            //  Здесь сохранение в файл, и потом инициализация через него
            clips.Clear();

            /*string stroka = codeBox.Text;
            System.IO.File.WriteAllText("tmp.clp", codeBox.Text);
            clips.Load("tmp.clp");*/

            //  Так тоже можно - без промежуточного вывода в файл
            clips.LoadFromString(codeBox.Text);

            clips.Reset();
        }

        private void openFile_Click(object sender, EventArgs e)
        {
            if (clipsOpenFileDialog.ShowDialog() == DialogResult.OK)
            {
                codeBox.Text = System.IO.File.ReadAllText(clipsOpenFileDialog.FileName);
                Text = "Экспертная система \"Авто\" – " + clipsOpenFileDialog.FileName;
                clips.Clear();
                clips.LoadFromString(codeBox.Text);
                clips.Reset();
            }
        }

        private void fontSelect_Click(object sender, EventArgs e)
        {
            if (fontDialog1.ShowDialog() == DialogResult.OK)
            {
                codeBox.Font = fontDialog1.Font;
                outputBox.Font = fontDialog1.Font;
            }
        }

        private void saveAsButton_Click(object sender, EventArgs e)
        {
            clipsSaveFileDialog.FileName = clipsOpenFileDialog.FileName;
            if (clipsSaveFileDialog.ShowDialog() == DialogResult.OK)
            {
                System.IO.File.WriteAllText(clipsSaveFileDialog.FileName, codeBox.Text);
            }
        }

        private void ChooseFacts(object sender, ItemCheckEventArgs e)
        {
            var listBox = sender as CheckedListBox;

            if (listBox.Name == "checkedListBox2")
                switch (e.Index)
                {
                    case 0:
                        listBox.SetItemChecked(1, false);
                        listBox.SetItemChecked(2, false);
                        break;
                    case 1:
                        listBox.SetItemChecked(0, false);
                        listBox.SetItemChecked(2, false);
                        break;
                    case 2:
                        listBox.SetItemChecked(0, false);
                        listBox.SetItemChecked(1, false);
                        break;
                }
        }
    }
}
