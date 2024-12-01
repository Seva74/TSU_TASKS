using lab13.Models;
using lab13.Services;
using Microsoft.AspNetCore.Mvc;
using System.Diagnostics;

namespace lab13.Controllers
{
    public class HomeController : Controller
    {
        private readonly ILogger<HomeController> _logger;

        private IQuizService _quizService;

        public HomeController(ILogger<HomeController> logger, IQuizService quizService)
        {
            _logger = logger;
            _quizService = quizService;
        }

        public IActionResult Index()
        {
            return View();
        }

        [HttpGet, ActionName("Quiz")]
        public IActionResult Quiz()
        {
            Quiz quiz = _quizService.getCurrentQuiz();
            quiz.reset();
            _quizService.addNewQuestion(quiz);

            return View("QuizQuestion", new QuizModel(quiz));
        }

        [HttpGet]
        public IActionResult GetResults()
        {
            Quiz quiz = _quizService.getCurrentQuiz();
            return View("Result", quiz);
        }

        [HttpPost, ActionName("Quiz")]
        public IActionResult Quiz(int answer, string action)
        {
            Quiz quiz = _quizService.getCurrentQuiz();

            if (!ModelState.IsValid)
            {
                QuizQuestion question = _quizService.getCurrentQuestion(quiz);
                return View("QuizQuestion", new QuizModel(quiz));
            }
            _quizService.answerLastQuestion(new QuizQuestionAnswer(answer), quiz);
            if (action == "next")
            {
                QuizQuestion question = _quizService.addNewQuestion(quiz);
                return View("QuizQuestion", new QuizModel(quiz));
            }
            return View("Result", quiz);
        }

        public IActionResult Privacy()
        {
            return View();
        }

        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }
    }
}