#include "mesosched/TupleParser.hpp"
#include "mesosched/Sched.hpp"
#include <string>

namespace mesosched {
    static std::set<Resource> resources(const PairVector& aNodeToResource)
    {
        std::set<Resource> resources;
        for (auto nodeResourcePair : aNodeToResource) {
            resources.emplace(nodeResourcePair.first, nodeResourcePair.second);
        }
        return resources;
    }

    static std::set<Job> jobs(const PairVector& aResourceToTimestep)
    {
        std::set<Job> jobs;
        for (auto it = aResourceToTimestep.begin(); it != aResourceToTimestep.end(); ++it)
        {
            jobs.emplace(it - aResourceToTimestep.begin(), (*it).first, (*it).second);
        }
        return jobs;
    }

    Resource Sched::resourceJustLargeEnoughToFit(size_t aRequired)
    {
        std::vector<Resource> resources{fResources.begin(), fResources.end()};
        auto minDeltaWithRequired = [aRequired](const Resource& aFirst, const Resource& aSecond) {
                return aFirst.fAvailable - aRequired < aSecond.fAvailable - aRequired;
            };
        std::sort(resources.begin(), resources.end(), minDeltaWithRequired);
        auto positiveDelta = [aRequired](const Resource& aResource) {
            return aResource.fAvailable - aRequired > 0;
        };
        auto foundResourceIt = std::find_if(resources.begin(), resources.end(), positiveDelta);
        if (foundResourceIt == resources.end()) { throw NodesTooSmall{}; }
        auto resource = *foundResourceIt;
        fResources.erase(resource);
        return resource;
    }

    NodeJobAllocation Sched::allocateOneTimestep()
    {
        NodeJobAllocation allocation;
        while (fResources.size() && fIncomingJobs.size()) {
            auto largestTimestep =
                [](const Job& aFirst, const Job& aSecond) {
                    return aFirst.fTimestep < aSecond.fTimestep;
                };
            auto jobIt = std::max_element(fIncomingJobs.begin(), fIncomingJobs.end(),
                                          largestTimestep);
            auto job = *jobIt;
            fIncomingJobs.erase(jobIt);
            try {
                auto resource = resourceJustLargeEnoughToFit(job.fResource);
                allocation.allocate(resource, job);
            } catch (NodesTooSmall&) {
                auto isBigEnoughForJob = [job](const std::pair<Resource, Job>& aAllocation) {
                    return job.fResource < aAllocation.first.fAvailable;
                };
                if (std::none_of(allocation.begin(), allocation.end(), isBigEnoughForJob)) {
                    throw;
                }
                fJobQueue.emplace(job);
                continue;
            }
        }
        fJobQueue.insert(fIncomingJobs.begin(), fIncomingJobs.end());
        return allocation;
    }

    Sched::Sched(const std::string&& aNodeToResource, const std::string&& aResourceToTimestep)
        : fNodeToResource(TupleParser::parse(std::move(aNodeToResource)))
        , fResourceToTimestep(TupleParser::parse(std::move(aResourceToTimestep)))
        , fResources(resources(fNodeToResource))
        , fIncomingJobs(jobs(fResourceToTimestep))
    {
        size_t timeStep = 1;
        while (fIncomingJobs.size()) {
            fNodeJobAllocationAtTimestep.emplace_back(timeStep, allocateOneTimestep());
            auto& currentAlloc = fNodeJobAllocationAtTimestep.front().second;
            std::set<Resource> freedResources;
            for (; freedResources.empty(); ++timeStep, freedResources = currentAlloc.step());
            fResources.insert(freedResources.begin(), freedResources.end());
            fIncomingJobs = fJobQueue;
            fJobQueue.clear();
        }
    }
}
